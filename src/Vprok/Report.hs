{-# LANGUAGE OverloadedStrings #-}

-- | Текстовые отчёты по списку товаров.
module Vprok.Report
  ( SortKey(..)
  , parseSortKey
  , sortProducts
  , renderReport
  , renderStock
  , formatDouble
  , writeTextFile
  ) where

import           Data.List            (partition, sortOn)
import           Data.Maybe           (fromMaybe)
import           Data.Ord             (Down (..))
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import qualified Data.ByteString      as BS
import           Numeric              (showFFloat)
import           Vprok.Types

data SortKey
  = SortAsIs
  | SortDiscount
  | SortPrice
  | SortRating
  | SortReviews
  deriving (Eq, Show)

parseSortKey :: String -> Either String SortKey
parseSortKey s = case s of
  "as-is"    -> Right SortAsIs
  "discount" -> Right SortDiscount
  "price"    -> Right SortPrice
  "rating"   -> Right SortRating
  "reviews"  -> Right SortReviews
  other      -> Left ("неизвестная сортировка: " ++ other
                       ++ " (допустимо: as-is, discount, price, rating, reviews)")

sortProducts :: SortKey -> [Product] -> [Product]
sortProducts key = case key of
  SortAsIs     -> id
  SortDiscount -> sortOn (Down . fromMaybe 0 . discountValue)
  SortPrice    -> sortOn (fromMaybe (1 / 0) . prodPrice)
  SortRating   -> sortOn (Down . fromMaybe 0 . prodRating)
  SortReviews  -> sortOn (Down . fromMaybe 0 . prodReviews)

-- | Формат совместим с products-api.txt из JS-версии.
renderReport :: [Product] -> Text
renderReport = T.concat . map block
  where
    block p = T.unlines
      [ "Название товара: " <> prodName p
      , "Ссылка на страницу товара: " <> productLink p
      , "Рейтинг: " <> maybe dash formatDouble (prodRating p)
      , "Количество отзывов: " <> maybe dash (T.pack . show) (prodReviews p)
      , "Цена: " <> maybe "Недоступен" formatDouble (prodPrice p)
      , "Акционная цена: " <> maybe dash formatDouble (salePrice p)
      , "Цена до акции: " <> maybe dash formatDouble (basePrice p)
      , "Размер скидки: " <> maybe dash formatDouble (discountValue p)
      , "---------------"
      ]

renderStock :: [Product] -> Text
renderStock products = T.unlines $
  [ "В наличии: " <> T.pack (show (length available))
      <> " из " <> T.pack (show (length products))
  , ""
  , "Нет в наличии:"
  ]
  ++ (if null missing then ["  (все товары доступны)"] else map bullet missing)
  where
    (available, missing) = partition inStock products
    bullet p = "  - " <> prodName p <> "\n    " <> productLink p

formatDouble :: Double -> Text
formatDouble d
  | d == fromIntegral rounded = T.pack (show rounded)
  | otherwise                 = T.pack (showFFloat (Just 2) d "")
  where
    rounded = round d :: Integer

dash :: Text
dash = "—"

-- | Пишем строго в UTF-8, не полагаясь на кодировку консоли Windows.
writeTextFile :: FilePath -> Text -> IO ()
writeTextFile path = BS.writeFile path . TE.encodeUtf8
