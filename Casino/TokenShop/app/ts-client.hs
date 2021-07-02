{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
    ( main
    ) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad                           (forM_, when)
import           Control.Monad.IO.Class                  (MonadIO (..))
import           Data.Aeson                              (Result (..), ToJSON, decode, encode, fromJSON)
import qualified Data.ByteString.Lazy.Char8              as B8
import qualified Data.ByteString.Lazy                    as LB
import           Data.Monoid                             (Last (..))
import           Data.Proxy                              (Proxy (..))
import           Data.String                             (IsString (..))
import           Data.Text                               (Text, pack)
import           Data.UUID                               hiding (fromString)
import           Ledger.Value                            (AssetClass (..), CurrencySymbol, Value, flattenValue, TokenName)
import           Network.HTTP.Req
import qualified Plutus.Contracts.Uniswap                as US
import           Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse (..))
import           Plutus.PAB.Webserver.Types
import           System.Environment                      (getArgs)
import           System.Exit                             (exitFailure)
import           Text.Printf                             (printf)
import           Text.Read                               (readMaybe)
import           Wallet.Emulator.Types                   (Wallet (..))

import           Shop.TokenShopPAB

main :: IO ()
main = do
    w   <- Wallet . read . head <$> getArgs
    cid <- read <$> readFile (cidFile w)
    go cid
  where
    go :: UUID -> IO a
    go cid = do
        cmd <- readCommandIO
        case cmd of
            SetPrice p -> setPrice cid p
            Funds -> getFunds cid
        go cid

data Command =
      Funds
    | SetPrice Integer
    | AddTokens Integer
    | BuyTokens Integer
    | Withdraw Integer Integer
    | Close ()
    deriving (Show, Read, Eq, Ord)

readCommandIO :: IO Command
readCommandIO = do
    putStrLn "Enter a command:"
    s <- getLine
    maybe readCommandIO return $ readMaybe s

getFunds :: UUID -> IO ()
getFunds cid = do
    callEndpoint cid "funds" ()
    threadDelay 2_000_000
    go
  where
    go = do
        f <- getStatus cid
        putStrLn $ "funds: " ++ show f

setPrice :: UUID -> Integer -> IO ()
setPrice cid p = do
    callEndpoint cid "set price" p
    threadDelay 2_000_000

getStatus :: UUID -> IO Value
getStatus cid = runReq defaultHttpConfig $ do
    w <- req
        GET
        (http "127.0.0.1" /: "api"  /: "new" /: "contract" /: "instance" /: pack (show cid) /: "status")
        NoReqBody
        (Proxy :: Proxy (JsonResponse (ContractInstanceClientState TokenShopContracts)))
        (port 8080)
    case fromJSON $ observableState $ cicCurrentState $ responseBody w of
        Success (Last Nothing)  -> liftIO $ threadDelay 1_000_000 >> getStatus cid
        Success (Last (Just f)) -> return f
        _                       -> liftIO $ ioError $ userError "error decoding state"

callEndpoint :: ToJSON a => UUID -> String -> a -> IO ()
callEndpoint cid name a = handle h $ runReq defaultHttpConfig $ do
    liftIO $ printf "\npost request to 127.0.1:8080/api/new/contract/instance/%s/endpoint/%s\n" (show cid) name
    liftIO $ printf "request body: %s\n\n" $ B8.unpack $ encode a
    v <- req
        POST
        (http "127.0.0.1" /: "api"  /: "new" /: "contract" /: "instance" /: pack (show cid) /: "endpoint" /: pack name)
        (ReqBodyJson a)
        (Proxy :: Proxy (JsonResponse ()))
        (port 8080)
    when (responseStatusCode v /= 200) $
        liftIO $ ioError $ userError $ "error calling endpoint " ++ name
  where
    h :: HttpException -> IO ()
    h = ioError . userError . show
