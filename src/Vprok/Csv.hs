{-# LANGUAGE OverloadedStrings #-}

-- | Накопление истории цен: каждый прогон дописывает строки в один CSV.
module Vprok.Csv
  ( appendPriceHistory
  ) where

import qualified Data.ByteString    as BS
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE
import           Data.Time          (UTCTime, defaultTimeLocale, formatTime)
import           System.Directory   (createDirectoryIfMissing, doesFileExist)
import           System.FilePath    (takeDirectory)
import           Vprok.Report       (formatDouble)
import           Vprok.Types

header :: Text
header = "fetched_at,category_id,name,url,price,old_price,discount,rating,reviews,in_stock"

appendPriceHistory :: FilePath -> UTCTime -> Text -> [Product] -> IO ()
appendPriceHistory path now categoryId products = do
  createDirectoryIfMissing True (takeDirectory path)
  exists <- doesFileExist path
  let prefix = if exists then "" else header <> "\n"
      rows   = T.unlines (map row products)
  BS.appendFile path (TE.encodeUtf8 (prefix <> rows))
  where
    stamp = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now)
    row p = T.intercalate ","
      [ quote stamp
      , quote categoryId
      , quote (prodName p)
      , quote (productLink p)
      , num (prodPrice p)
      , num (basePrice p)
      , num (discountValue p)
      , num (prodRating p)
      , maybe "" (T.pack . show) (prodReviews p)
      , if inStock p then "1" else "0"
      ]
    num = maybe "" formatDouble

-- | Кавычки внутри поля удваиваются — это весь CSV-экранаж, который нужен.
quote :: Text -> Text
quote t = "\"" <> T.replace "\"" "\"\"" t <> "\""
