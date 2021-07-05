{-# LANGUAGE OverloadedStrings #-}

module Helpers
    ( performReq
    ) where

import Network.HTTP.Req
import Network.Wai

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import           Control.Monad                           (forM_, when)
import           Control.Monad.IO.Class                  (MonadIO (..))

performReq :: Request -> IO ()
performReq r = runReq defaultHttpConfig $ do
  let route = "localhost" `T.append` (TE.decodeUtf8 $ rawPathInfo r)
  liftIO $ T.putStrLn route
  reqBody <- liftIO $ getRequestBodyChunk r
  w <- req POST (http route) (ReqBodyBs reqBody) bsResponse (port 8080)
  liftIO $ B8.putStrLn $ responseBody w
  return ()

