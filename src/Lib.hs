{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( run
    , Service (..)
    ) where

import           Control.Concurrent      (forkIO)
import           Control.Exception       (try)
import           Control.Lens            ((&), (.~), (^.))
import           Control.Monad           (forever, void)
import           Data.Aeson              (FromJSON (..), ToJSON (..),
                                          Value (..), object, withObject, (.!=),
                                          (.:), (.:?), (.=))
import qualified Data.Aeson              as A (decode, encode)
import           Data.Binary             (Binary (..))
import qualified Data.Binary             as B (decode, encode)
import           Data.Binary.Get         (getLazyByteString,
                                          getRemainingLazyByteString,
                                          getWord32be)
import           Data.Binary.Put         (putLazyByteString, putWord8)
import qualified Data.ByteString         as B (ByteString)
import qualified Data.ByteString.Char8   as B (unpack)
import           Data.ByteString.Lazy    (ByteString)
import qualified Data.ByteString.Lazy    as LB (fromStrict, length, null,
                                                toStrict)
import           Data.CaseInsensitive    (mk)
import           Data.Char               (toLower, toUpper)
import           Data.HashMap.Strict     (foldrWithKey)
import           Data.Maybe              (fromMaybe)
import           Data.String.Utils       (startswith)
import           Data.Text               (Text)
import           Data.Text.Encoding      (decodeUtf8, encodeUtf8)
import           Network.HTTP.Client     (HttpException (..),
                                          HttpExceptionContent (..))
import           Network.HTTP.Client     (Manager, defaultManagerSettings,
                                          managerConnCount,
                                          managerResponseTimeout, newManager,
                                          responseTimeoutMicro)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.HTTP.Types      as N (statusCode)
import           Network.WebSockets      (DataMessage (..), ServerApp,
                                          WebSocketsData (..), acceptRequest,
                                          receiveData, runServer,
                                          sendBinaryData, sendTextData)
import           Network.Wreq            (Options, customMethodWith,
                                          customPayloadMethodWith, defaults,
                                          header, manager, responseBody,
                                          responseHeader, responseStatus)

type ServiceName = String

data Service = Service
  { serviceName :: ServiceName
  , serviceMgr  :: Manager
  , serviceHost :: String
  }

instance FromJSON Service where
  parseJSON = withObject "Service" $ \o -> do
    serviceName <- o .: "service"
    serviceHost <- o .: "host"
    return Service {serviceMgr = error "no initial", ..}


data DataType = BINARY | TEXT
  deriving (Show)

data WsRequest = WsRequest
  { reqid      :: String
  , service    :: ServiceName
  , pathname   :: String
  , method     :: String
  , reqHeaders :: Value
  , reqBody    :: ByteString
  }
  deriving (Show)

instance FromJSON WsRequest where
  parseJSON = withObject "WsRequest" $ \o -> do
    reqid      <- o .: "reqid"
    service    <- o .: "service"
    pathname   <- o .:? "pathname" .!= "/"
    method     <- o .:? "method"   .!= "GET"
    reqHeaders <- o .:? "headers"  .!= Null
    body       <- o .:? "body"     .!= ""
    return WsRequest
      { reqBody = LB.fromStrict $ encodeUtf8 body
      , ..
      }

instance Binary WsRequest where
  get = do
    h <- getWord32be
    bs <- getLazyByteString $ fromIntegral h
    case A.decode bs of
      Nothing -> fail "decode error"
      Just wq -> do
        b <- getRemainingLazyByteString
        return wq {reqBody = b}

  put = error "no implement"

defaultWsRequest :: WsRequest
defaultWsRequest = WsRequest
  { reqid      = "unkonw"
  , service    = "unkonw"
  , pathname   = "/"
  , method     = "GET"
  , reqHeaders = Null
  , reqBody    = ""
  }

instance WebSocketsData WsRequest where
  fromDataMessage (Text bl _) = fromMaybe defaultWsRequest $ A.decode bl
  fromDataMessage (Binary bl) = B.decode bl
  fromLazyByteString bl       = B.decode bl
  toLazyByteString _          = error "no implement"

data WsResponse = WsResponse
  { resid       :: String
  , statusCode  :: Int
  , contentType :: B.ByteString
  , resType     :: DataType
  , resBody     :: ByteString
  }
  deriving (Show)

instance ToJSON WsResponse where
  toJSON WsResponse{..} = object $
    [ "resid"       .= resid
    , "statusCode"  .= statusCode
    , "contentType" .= decodeUtf8 contentType
    ] ++ case resType of
           TEXT -> ["body" .= decodeUtf8 (LB.toStrict resBody)]
           _    -> []

instance Binary WsResponse where
  get = error "no implement"
  put w@(WsResponse {..}) = do
    putWord8 $ fromIntegral $ LB.length h
    putLazyByteString h
    putLazyByteString resBody
    where h = A.encode w

fix :: String -> String
fix ('/':xs) = '/' : xs
fix xs       = '/' : xs

upper :: String -> String
upper = map toUpper

lower :: String -> String
lower = map toLower

getDataType :: B.ByteString -> DataType
getDataType bs
  | startswith "application/json" s = TEXT
  | startswith "text"             s = TEXT
  | otherwise = BINARY
  where s = lower $ B.unpack bs

addDataType :: WsResponse -> WsResponse
addDataType ws | LB.null (resBody ws) = ws { resType=TEXT }
               | otherwise = ws {resType = getDataType $ contentType ws}

request :: (ServiceName -> Maybe (String, Manager)) -> WsRequest -> IO WsResponse
request f (WsRequest {..}) = do
  case f service of
    Nothing -> return res
    Just (host, mgr) -> do
      let
        url = host ++ fix pathname
        opts = mergeHeaders (defaults & manager .~ Right mgr) reqHeaders

        m = upper method

        req = if LB.null reqBody then customMethodWith m opts url
                                 else customPayloadMethodWith m opts url reqBody

      e <- try req
      case e of
        Left (HttpExceptionRequest _ content) ->
          case content of
            (StatusCodeException r dat) -> do
              let st = r ^. responseStatus
                  ct = r ^. responseHeader "Content-Type"

              return res { contentType = ct, statusCode = N.statusCode st, resBody = LB.fromStrict dat }
            ResponseTimeout -> do
              return res { contentType = "raw", statusCode = 504, resBody = "" }
            _ -> do
              return res { contentType = "raw", statusCode = 502, resBody = "" }

        Left (InvalidUrlException _ _) -> do
          return res { contentType = "raw", statusCode = 500, resBody = "" }
        Right r  -> do
          let st  = r ^. responseStatus
              ct  = r ^. responseHeader "Content-Type"
              dat = r ^. responseBody

          return res { contentType = ct, statusCode = N.statusCode st, resBody = dat }

  where res = WsResponse
               { resid = reqid
               , statusCode = 200
               , resType = TEXT
               , contentType = "application/json"
               , resBody = A.encode $ object ["err" .= ("Not Support" :: String)]
               }

mergeHeaders :: Options -> Value -> Options
mergeHeaders opts (Object hm) = foldrWithKey foldFunc opts hm
  where foldFunc :: Text -> Value -> Options -> Options
        foldFunc k (String v) opt = opt & header (mk $ encodeUtf8 k) .~ [encodeUtf8 v]
        foldFunc _ _ opt          = opt

mergeHeaders opts _ = opts

wsApp :: (ServiceName -> Maybe (String, Manager)) -> ServerApp
wsApp f pending_conn = do
    conn <- acceptRequest pending_conn
    forever $ do
      req <- receiveData conn :: IO WsRequest
      void $ forkIO $ do
        res <- addDataType <$> request f req
        case resType res of
          TEXT   -> sendTextData conn $ A.encode res
          BINARY -> sendBinaryData conn $ B.encode res

run :: String -> Int -> [Service] -> IO ()
run host port srvs = do
  srvs' <- mapM initService srvs
  runServer host port (wsApp (getService srvs'))

getService :: [Service] -> ServiceName -> Maybe (String, Manager)
getService [] _ = Nothing
getService (Service {..}:xs) n
  | serviceName == n = Just (serviceHost, serviceMgr)
  | otherwise = getService xs n

initService :: Service -> IO Service
initService srv = do
  mgr <- initMgr $ serviceHost srv
  return srv {serviceMgr = mgr}

initMgr :: String -> IO Manager
initMgr root = newManager settings
  { managerConnCount = 1000
  , managerResponseTimeout = responseTimeoutMicro $ 300 * 1000000
  }

  where settings = if startswith "https" root then tlsManagerSettings
                                              else defaultManagerSettings
