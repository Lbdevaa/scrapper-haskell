{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           Data.Time           (getCurrentTime)
import           Options.Applicative
import           System.Exit         (die)
import           System.IO           (hSetEncoding, stderr, stdout, utf8)

import           Vprok.Api
import           Vprok.Csv           (appendPriceHistory)
import           Vprok.Diff          (diffProducts, renderDiff)
import           Vprok.Report
import           Vprok.Snapshot
import           Vprok.Types

data Command
  = CmdFetch FetchArgs
  | CmdReport ReportArgs
  | CmdDiff FilePath FilePath
  | CmdStock FilePath

data FetchArgs = FetchArgs
  { faCategory    :: Text
  , faLimit       :: Int
  , faPage        :: Int
  , faSort        :: Text
  , faSnapshotDir :: FilePath
  , faReport      :: Maybe FilePath
  , faCsv         :: Maybe FilePath
  , faTop         :: Maybe Int
  , faOrder       :: SortKey
  }

data ReportArgs = ReportArgs
  { raSnapshot :: FilePath
  , raTop      :: Maybe Int
  , raOrder    :: SortKey
  , raOut      :: Maybe FilePath
  }

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  cmd <- execParser cli
  case cmd of
    CmdFetch args      -> runFetch args
    CmdReport args     -> runReport args
    CmdDiff before now -> runDiff before now
    CmdStock path      -> runStock path

cli :: ParserInfo Command
cli = info (commands <**> helper)
  ( fullDesc
 <> header "scrapper — выгрузка каталога vprok.ru: снапшоты, отчёты, история цен"
  )

commands :: Parser Command
commands = subparser
  ( command "fetch"
      (info (CmdFetch <$> fetchArgs <**> helper)
            (progDesc "Скачать категорию, сохранить снапшот, отчёт и историю цен"))
 <> command "report"
      (info (CmdReport <$> reportArgs <**> helper)
            (progDesc "Собрать текстовый отчёт из сохранённого снапшота"))
 <> command "diff"
      (info (CmdDiff <$> strArgument (metavar "СТАРЫЙ")
                     <*> strArgument (metavar "НОВЫЙ")
                     <**> helper)
            (progDesc "Сравнить два снапшота: цены, наличие, состав"))
 <> command "stock"
      (info (CmdStock <$> strArgument (metavar "СНАПШОТ") <**> helper)
            (progDesc "Показать, чего нет в наличии"))
  )

fetchArgs :: Parser FetchArgs
fetchArgs = FetchArgs
  <$> strArgument
      ( metavar "КАТЕГОРИЯ"
     <> help "Ссылка на категорию, путь каталога или числовой идентификатор" )
  <*> option auto
      ( long "limit" <> metavar "N" <> value 30 <> showDefault
     <> help "Сколько товаров запросить" )
  <*> option auto
      ( long "page" <> metavar "N" <> value 1 <> showDefault
     <> help "Номер страницы" )
  <*> strOption
      ( long "sort" <> metavar "S" <> value "popularity_desc" <> showDefault
     <> help "Сортировка на стороне API" )
  <*> strOption
      ( long "snapshots" <> metavar "DIR" <> value "snapshots" <> showDefault
     <> help "Куда складывать сырые ответы API" )
  <*> optional (strOption
      ( long "report" <> metavar "FILE"
     <> help "Записать текстовый отчёт" ))
  <*> optional (strOption
      ( long "csv" <> metavar "FILE"
     <> help "Дописать строки в историю цен" ))
  <*> optional (option auto
      ( long "top" <> metavar "N"
     <> help "Оставить в отчёте только первые N товаров" ))
  <*> orderOption

reportArgs :: Parser ReportArgs
reportArgs = ReportArgs
  <$> strArgument (metavar "СНАПШОТ" <> help "Путь к сохранённому снапшоту")
  <*> optional (option auto
      ( long "top" <> metavar "N" <> help "Оставить только первые N товаров" ))
  <*> orderOption
  <*> optional (strOption
      ( long "out" <> short 'o' <> metavar "FILE"
     <> help "Записать отчёт в файл вместо вывода на экран" ))

orderOption :: Parser SortKey
orderOption = option (eitherReader parseSortKey)
  ( long "order" <> metavar "KEY" <> value SortAsIs
 <> help "Порядок: as-is, discount, price, rating, reviews (по умолчанию as-is)" )

runFetch :: FetchArgs -> IO ()
runFetch args = do
  ref <- orDie (parseCategoryRef (faCategory args))
  let opts = FetchOptions { foLimit = faLimit args
                          , foPage  = faPage args
                          , foSort  = faSort args
                          }
  TIO.putStrLn ("Запрашиваю категорию " <> catId ref <> " (" <> catPath ref <> ")")
  payload  <- orDie =<< fetchCategory opts ref
  now      <- getCurrentTime
  let snap = Snapshot (Just now) (catId ref) (catPath ref) payload
  products <- orDie (snapshotProducts snap)

  path <- saveSnapshot (faSnapshotDir args) snap
  putStrLn ("Снапшот: " ++ path)

  let picked = takeTop (faTop args) (sortProducts (faOrder args) products)

  case faReport args of
    Nothing   -> pure ()
    Just file -> do
      writeTextFile file (renderReport picked)
      putStrLn ("Отчёт: " ++ file)

  case faCsv args of
    Nothing   -> pure ()
    Just file -> do
      appendPriceHistory file now (catId ref) products
      putStrLn ("История цен дополнена: " ++ file)

  TIO.putStr (summary products)

runReport :: ReportArgs -> IO ()
runReport args = do
  products <- loadProducts (raSnapshot args)
  let text = renderReport (takeTop (raTop args) (sortProducts (raOrder args) products))
  case raOut args of
    Nothing   -> TIO.putStr text
    Just file -> writeTextFile file text >> putStrLn ("Отчёт: " ++ file)

runDiff :: FilePath -> FilePath -> IO ()
runDiff beforePath afterPath = do
  before <- loadProducts beforePath
  after  <- loadProducts afterPath
  TIO.putStr (renderDiff (diffProducts before after))

runStock :: FilePath -> IO ()
runStock path = do
  products <- loadProducts path
  TIO.putStr (renderStock products)

loadProducts :: FilePath -> IO [Product]
loadProducts path = do
  snap <- orDieWith ("не удалось прочитать " ++ path) =<< loadSnapshot path
  orDieWith ("не удалось разобрать товары в " ++ path) (snapshotProducts snap)

summary :: [Product] -> Text
summary products = T.unlines
  [ "Товаров: " <> T.pack (show (length products))
  , "В наличии: " <> T.pack (show (length (filter inStock products)))
  , "Со скидкой: " <> T.pack (show (length (filter hasDiscount products)))
  ]
  where
    hasDiscount p = case discountValue p of
      Just d  -> d > 0
      Nothing -> False

takeTop :: Maybe Int -> [a] -> [a]
takeTop = maybe id take

orDie :: Either String a -> IO a
orDie = either die pure

orDieWith :: String -> Either String a -> IO a
orDieWith context = either (\err -> die (context ++ ": " ++ err)) pure
