{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai (responseLBS, requestMethod)
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import Control.Monad.IO.Class
import Data.String.Conversions

import Helpers

main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    let settings = setPort port $ setHost "127.0.0.1" defaultSettings
    runSettings settings app

app req f = do
  liftIO $ putStrLn "Forwarding request..."
  (headers, status, body) <- performReq req
  f $ responseLBS status (headers ++ [("Access-Control-Allow-Origin", "*")]) $ cs body
