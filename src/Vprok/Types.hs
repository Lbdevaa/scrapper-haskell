{-# LANGUAGE OverloadedStrings #-}

-- | Модель товара из каталога vprok.ru и производные от неё величины.
module Vprok.Types
  ( Product(..)
  , CategoryPayload(..)
  , productLink
  , basePrice
  , discountValue
  , inStock
  ) where

import           Data.Aeson
import           Data.Aeson.Types  (Parser, typeMismatch)
import           Data.Text         (Text)
import qualified Data.Text         as T
import           Text.Read         (readMaybe)

data Product = Product
  { prodName     :: Text
  , prodUrl      :: Text
  , prodRating   :: Maybe Double
  , prodReviews  :: Maybe Int
  , prodPrice    :: Maybe Double
  , prodOldPrice :: Maybe Double
  , prodDiscount :: Maybe Double
  } deriving (Eq, Show)

newtype CategoryPayload = CategoryPayload
  { payloadProducts :: [Product]
  } deriving (Eq, Show)

instance FromJSON Product where
  parseJSON = withObject "Product" $ \o ->
    Product
      <$> o .:  "name"
      <*> o .:  "url"
      <*> optDouble o "rating"
      <*> optInt    o "reviews"
      <*> optDouble o "price"
      <*> optDouble o "oldPrice"
      <*> optDouble o "discount"

instance ToJSON Product where
  toJSON p = object
    [ "name"     .= prodName p
    , "url"      .= prodUrl p
    , "rating"   .= prodRating p
    , "reviews"  .= prodReviews p
    , "price"    .= prodPrice p
    , "oldPrice" .= prodOldPrice p
    , "discount" .= prodDiscount p
    ]

instance FromJSON CategoryPayload where
  parseJSON = withObject "CategoryPayload" $ \o ->
    CategoryPayload <$> o .: "products"

-- | API непоследователен: одно и то же поле приходит то числом, то строкой,
-- то null. Разбираем все три случая, чтобы прогон не падал целиком из-за
-- одного товара.
asDouble :: Value -> Parser Double
asDouble (Number n) = pure (realToFrac n)
asDouble (String s) =
  case readMaybe (T.unpack (T.replace "," "." (T.strip s))) of
    Just d  -> pure d
    Nothing -> fail ("не удалось разобрать число: " ++ T.unpack s)
asDouble v = typeMismatch "Number or String" v

optDouble :: Object -> Key -> Parser (Maybe Double)
optDouble o k = do
  mv <- o .:? k
  case mv of
    Nothing   -> pure Nothing
    Just Null -> pure Nothing
    Just v    -> Just <$> asDouble v

optInt :: Object -> Key -> Parser (Maybe Int)
optInt o k = fmap (round :: Double -> Int) <$> optDouble o k

-- | В API лежит относительный путь, иногда с ведущим слэшем: срезаем его,
-- чтобы в ссылке не получилось двойного слэша.
productLink :: Product -> Text
productLink p = "https://www.vprok.ru/" <> T.dropWhile (== '/') (prodUrl p)

-- | Цена до акции — только если она осмысленная, то есть выше текущей.
basePrice :: Product -> Maybe Double
basePrice p =
  case (prodOldPrice p, prodPrice p) of
    (Just old, Just now) | old > 0 && old > now -> Just old
    (Just old, Nothing)  | old > 0              -> Just old
    _                                           -> Nothing

-- | Размер скидки: берём из API, а если его там нет — считаем сами.
discountValue :: Product -> Maybe Double
discountValue p =
  case prodDiscount p of
    Just d | d > 0 -> Just d
    _ -> case (basePrice p, prodPrice p) of
           (Just old, Just now) -> Just (old - now)
           _                    -> Nothing

-- | Каталог не отдаёт флага наличия: у недоступного товара просто нет цены.
inStock :: Product -> Bool
inStock p =
  case prodPrice p of
    Just price -> price > 0
    Nothing    -> False
