{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( run
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
import           Data.ByteString.Lazy    as LB (fromStrict, length)
import           Data.ByteString.Lazy    (ByteString, empty)
import           Data.CaseInsensitive    (mk)
import           Data.HashMap.Strict     (foldrWithKey)
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
                                          sendBinaryData)
import           Network.Wreq            (Options, customPayloadMethodWith,
                                          defaults, header, manager,
                                          responseBody, responseHeader,
                                          responseStatus)

type Service = String

data WsRequest = WsRequest
  { reqid      :: String
  , service    :: Service
  , pathname   :: String
  , method     :: String
  , reqHeaders :: Value
  , reqBody    :: ByteString
  }

instance FromJSON WsRequest where
  parseJSON = withObject "WsRequest" $ \o -> do
    reqid      <- o .: "reqid"
    service    <- o .: "service"
    pathname   <- o .:? "pathname" .!= "/"
    method     <- o .:? "method"   .!= "GET"
    reqHeaders <- o .:? "headers"  .!= Null
    return WsRequest{ reqBody = empty, .. }

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

instance WebSocketsData WsRequest where
  fromDataMessage (Text bl _) = B.decode bl
  fromDataMessage (Binary bl) = B.decode bl
  fromLazyByteString bl       = B.decode bl
  toLazyByteString _          = error "no implement"

data WsResponse = WsResponse
  { resid       :: String
  , statusCode  :: Int
  , contentType :: B.ByteString
  , resBody     :: ByteString
  }

instance ToJSON WsResponse where
  toJSON WsResponse{..} = object
    [ "resid"       .= resid
    , "statusCode"  .= statusCode
    , "contentType" .= decodeUtf8 contentType
    ]

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

request :: (Service -> Maybe (String, Manager)) -> WsRequest -> IO WsResponse
request f (WsRequest {..}) = do
  case f service of
    Nothing -> return res
    Just (host, mgr) -> do
      let
        url = host ++ fix pathname
        opts = mergeHeaders (defaults & manager .~ Right mgr) reqHeaders

      e <- try $ customPayloadMethodWith method opts url reqBody
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
               , contentType = "application/json"
               , resBody = A.encode $ object ["err" .= ("Not Support" :: String)]
               }

mergeHeaders :: Options -> Value -> Options
mergeHeaders opts (Object hm) = foldrWithKey foldFunc opts hm
  where foldFunc :: Text -> Value -> Options -> Options
        foldFunc k (String v) opt = opt & header (mk $ encodeUtf8 k) .~ [encodeUtf8 v]
        foldFunc _ _ opt          = opt

mergeHeaders opts _ = opts

wsApp :: (Service -> Maybe (String, Manager)) -> ServerApp
wsApp f pending_conn = do
    conn <- acceptRequest pending_conn
    forever $ do
      req <- receiveData conn :: IO WsRequest
      void $ forkIO $ do
        res <- request f req
        sendBinaryData conn $ B.encode res

run :: String -> IO ()
run root = do
  mgr <- initMgr root
  runServer "127.0.0.1" 3500 (wsApp (getService root mgr))

getService :: String -> Manager -> Service -> Maybe (String, Manager)
getService host mgr _ = Just (host, mgr)

initMgr :: String -> IO Manager
initMgr root = newManager settings
  { managerConnCount = 100
  , managerResponseTimeout = responseTimeoutMicro $ 300 * 1000000
  }

  where settings = if startswith "https" root then tlsManagerSettings
                                              else defaultManagerSettings
