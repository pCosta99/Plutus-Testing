{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Helpers
    ( performReq
    ) where

import Network.HTTP.Req
import Network.Wai
import qualified Text.URI as URI
import qualified Network.HTTP.Client as L
import qualified Network.HTTP.Types as T

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import           Control.Monad                           (forM_, when)
import           Control.Monad.IO.Class                  (MonadIO (..))

import Data.Maybe

{-
   Helper functions
-}

-- | Builds the URI we want to query and handles errors on it properly.
buildURI :: B.ByteString -> Req (Url 'Http)
buildURI endpoint = do
  let route = "http://127.0.0.1" `T.append` (TE.decodeUtf8 endpoint)
  uri <- useHttpURI <$> URI.mkURI route -- build the route in a format usable by Network.HTTP.Req
  case uri of
    Just (url, _) -> return url
    Nothing -> error "Invalid endpoint!"

-- | Auxiliar function that extracts the needed data from the response.
-- The purpose of this function is to obtain all data needed to re-send the response back to the client of the proxy.
extractRelevantData :: BsResponse -> Req (T.ResponseHeaders, T.Status, B8.ByteString)
extractRelevantData w = do
  let rh = L.responseHeaders $ toVanillaResponse w
      rs = L.responseStatus $ toVanillaResponse w
      rb = responseBody w
  return (rh, rs, rb)

------------------------------------------------------------------------------------------------------------------

-- | Checks what type of request was provided and forwards it to the corresponding function.
performReq :: Request -> IO (T.ResponseHeaders, T.Status, B8.ByteString)
performReq r = case requestMethod r of
                 "GET" -> performGet r
                 "POST" -> performPost r
                 _ -> ioError $ userError $ B8.unpack $ "Invalid request method: " `B.append` requestMethod r

-- | Performs the requested POST.
performPost :: Request -> IO (T.ResponseHeaders, T.Status, B8.ByteString)
performPost r = runReq defaultHttpConfig $ do
  uri <- buildURI $ rawPathInfo r
  reqBody <- liftIO $ getRequestBodyChunk r
  let body = if B.null reqBody then ReqBodyJson () else undefined -- Infer from the body what we need to do (needs fixing)
  w <- req POST uri (ReqBodyJson ()) bsResponse (port 8080) -- Perform the request to the endpoint
  extractRelevantData w

-- | Performs the requested GET.
performGet :: Request -> IO (T.ResponseHeaders, T.Status, B8.ByteString)
performGet r = runReq defaultHttpConfig $ do
  uri <- buildURI $ rawPathInfo r
  w <- req GET uri NoReqBody bsResponse (port 8080)
  extractRelevantData w
