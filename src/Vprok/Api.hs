{-# LANGUAGE OverloadedStrings #-}

-- | Запрос к каталогу vprok.ru. Тот же вызов, что делает страница каталога,
-- только без браузера: куки берём с главной обычным GET-ом.
module Vprok.Api
  ( CategoryRef(..)
  , FetchOptions(..)
  , defaultFetchOptions
  , parseCategoryRef
  , fetchCategory
  ) where

import           Data.Aeson                 (Value, eitherDecode, encode,
                                             object, (.=))
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Char                  (isDigit, isSpace)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS    (newTlsManager)
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status  (statusCode)

-- | Категория: числовой идентификатор для адреса запроса и путь каталога,
-- который сайт ждёт в теле POST-а.
data CategoryRef = CategoryRef
  { catId   :: Text
  , catPath :: Text
  } deriving (Eq, Show)

data FetchOptions = FetchOptions
  { foLimit :: Int
  , foPage  :: Int
  , foSort  :: Text
  } deriving (Eq, Show)

defaultFetchOptions :: FetchOptions
defaultFetchOptions = FetchOptions
  { foLimit = 30
  , foPage  = 1
  , foSort  = "popularity_desc"
  }

-- | Принимаем и полную ссылку на категорию, и путь, и голый идентификатор.
parseCategoryRef :: Text -> Either String CategoryRef
parseCategoryRef raw
  | T.null trimmed = Left "пустая ссылка на категорию"
  | T.all isDigit trimmed = Right (CategoryRef trimmed ("/catalog/" <> trimmed))
  | otherwise =
      case break (== "catalog") segments of
        (_, _ : ident : rest)
          | T.all isDigit ident && not (T.null ident) ->
              Right (CategoryRef ident (T.intercalate "/" ("" : "catalog" : ident : take 1 rest)))
        _ -> Left ("не удалось найти идентификатор категории в " ++ T.unpack raw)
  where
    trimmed  = T.strip raw
    stripped = dropHost trimmed
    segments = filter (not . T.null) (T.splitOn "/" stripped)

dropHost :: Text -> Text
dropHost t =
  case T.breakOn "://" t of
    (_, rest) | not (T.null rest) -> T.dropWhile (/= '/') (T.drop 3 rest)
    _                             -> t

fetchCategory :: FetchOptions -> CategoryRef -> IO (Either String Value)
fetchCategory opts ref = do
  manager <- newTlsManager

  -- Первый заход на главную: сайт выдаёт куки сессии и региона, без них
  -- API отвечает редиректом.
  homeReq <- parseRequest "https://www.vprok.ru/"
  homeResp <- httpLbs homeReq { requestHeaders = browserHeaders
                              , cookieJar      = Just (createCookieJar [])
                              } manager

  apiReq <- parseRequest (T.unpack (endpoint opts ref))
  let payload = encode (object [ "noRedirect" .= True
                               , "url"        .= catPath ref
                               ])
      req = apiReq { method          = "POST"
                   , requestBody     = RequestBodyLBS payload
                   , requestHeaders  = apiHeaders ref
                   , cookieJar       = Just (responseCookieJar homeResp)
                   }
  resp <- httpLbs req manager
  let status = statusCode (responseStatus resp)
      body   = responseBody resp
  pure $ case () of
    _ | status < 200 || status >= 300 -> Left ("API ответил кодом " ++ show status)
      | looksLikeHtml body -> Left (htmlHint body)
      | otherwise -> eitherDecode body

-- | Вместо JSON сайт может отдать HTML-страницу: защита от ботов, капча или
-- редирект на обычную вёрстку. Сообщаем об этом прямо, а не ошибкой разбора.
looksLikeHtml :: BL.ByteString -> Bool
looksLikeHtml body =
  case BLC.uncons (BLC.dropWhile isSpace body) of
    Just (c, _) -> c == '<'
    Nothing     -> False

htmlHint :: BL.ByteString -> String
htmlHint body = concat
  [ "вместо JSON пришла HTML-страница — похоже, запрос отклонён защитой сайта"
  , "; начало ответа: "
  , BLC.unpack (BL.take 120 body)
  ]

endpoint :: FetchOptions -> CategoryRef -> Text
endpoint opts ref = T.concat
  [ "https://www.vprok.ru/web/api/v1/catalog/category/", catId ref
  , "?sort=", foSort opts
  , "&limit=", T.pack (show (foLimit opts))
  , "&page=", T.pack (show (foPage opts))
  ]

browserHeaders :: RequestHeaders
browserHeaders =
  [ (hUserAgent, "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")
  , (hAcceptLanguage, "ru-RU,ru;q=0.9,en;q=0.8")
  ]

apiHeaders :: CategoryRef -> RequestHeaders
apiHeaders ref = browserHeaders ++
  [ (hContentType, "application/json")
  , (hAccept, "application/json, text/plain, */*")
  , ("Origin", "https://www.vprok.ru")
  , (hReferer, TE.encodeUtf8 ("https://www.vprok.ru" <> catPath ref))
  , ("X-Requested-With", "XMLHttpRequest")
  ]
