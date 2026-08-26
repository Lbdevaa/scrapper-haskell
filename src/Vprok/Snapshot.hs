{-# LANGUAGE OverloadedStrings #-}

-- | Снапшот прогона: сырой ответ API плюс метаданные. Храним именно сырой
-- JSON — из него всегда соберётся текстовый отчёт, обратно уже нет.
module Vprok.Snapshot
  ( Snapshot(..)
  , snapshotPath
  , saveSnapshot
  , loadSnapshot
  , snapshotProducts
  ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Time            (UTCTime, defaultTimeLocale, formatTime)
import           System.Directory     (createDirectoryIfMissing)
import           System.FilePath      (takeDirectory, (<.>), (</>))
import           Vprok.Types          (CategoryPayload (..), Product)

data Snapshot = Snapshot
  { snapFetchedAt  :: Maybe UTCTime
  , snapCategoryId :: Text
  , snapSourceUrl  :: Text
  , snapPayload    :: Value
  } deriving (Eq, Show)

instance ToJSON Snapshot where
  toJSON s = object
    [ "fetchedAt"  .= snapFetchedAt s
    , "categoryId" .= snapCategoryId s
    , "sourceUrl"  .= snapSourceUrl s
    , "payload"    .= snapPayload s
    ]

instance FromJSON Snapshot where
  parseJSON = withObject "Snapshot" $ \o -> do
    fetchedAt  <- o .:? "fetchedAt"
    categoryId <- (o .:? "categoryId") .!= ""
    sourceUrl  <- (o .:? "sourceUrl") .!= ""
    payload    <- o .: "payload"
    pure (Snapshot fetchedAt categoryId sourceUrl payload)

snapshotPath :: FilePath -> Text -> UTCTime -> FilePath
snapshotPath dir categoryId now =
  dir </> T.unpack categoryId </> stamp <.> "json"
  where
    stamp = formatTime defaultTimeLocale "%Y%m%d-%H%M%S" now

saveSnapshot :: FilePath -> Snapshot -> IO FilePath
saveSnapshot dir snap = do
  let now  = snapFetchedAt snap
      path = case now of
        Just t  -> snapshotPath dir (snapCategoryId snap) t
        Nothing -> dir </> T.unpack (snapCategoryId snap) </> "latest.json"
  createDirectoryIfMissing True (takeDirectory path)
  BL.writeFile path (encode snap)
  pure path

-- | Читаем и наш снапшот, и просто сохранённый ответ API — удобно, когда
-- файл пришёл со стороны.
loadSnapshot :: FilePath -> IO (Either String Snapshot)
loadSnapshot path = do
  bytes <- BL.readFile path
  pure $ case eitherDecode bytes of
    Right snap -> Right snap
    Left err   -> case eitherDecode bytes of
      Right value -> Right (Snapshot Nothing "" "" value)
      Left _      -> Left err

snapshotProducts :: Snapshot -> Either String [Product]
snapshotProducts snap =
  case fromJSON (snapPayload snap) of
    Success payload -> Right (payloadProducts payload)
    Error err       -> Left err
