{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE NumericUnderscores #-}

module Main
    ( main
    ) where

import           Control.Monad                       (forM_, void)
import           Control.Monad.Freer                 (Eff, Member, interpret, type (~>))
import           Control.Monad.Freer.Error           (Error)
import           Control.Monad.Freer.Extras.Log      (LogMsg)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (Result (..), fromJSON, FromJSON)
import qualified Data.Monoid                         as Monoid
import           Data.Text                           (Text)
import           Plutus.Contract
import           Plutus.PAB.Effects.Contract         (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), type (.\\))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Monitoring.PABLogMsg     (PABMultiAgentMsg)
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers, logString)
import qualified Plutus.PAB.Simulator                as Simulator
import           Plutus.PAB.Types                    (PABError (..))
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import           Prelude                             hiding (init)
import           Wallet.Emulator.Types               (Wallet (..))
import           Wallet.Types                        (ContractInstanceId (..))
import qualified Ledger.Value                        as Value

import Shop.OffChain
import Shop.TokenShopPAB
import Shop.Types

main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    logString @(Builtin TokenShopContracts) "Starting TokenShop PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    cidTS <- Simulator.activateContract (Wallet 1) TokenShopStart
    logString @(Builtin TokenShopContracts) "Started running token shop!"
    liftIO $ writeFile "ts.cid" $ show $ unContractInstanceId cidTS
    ts <- waitForLast cidTS
    _  <- Simulator.waitUntilFinished cidTS

    forM_ wallets $ \w -> do
        cid <- Simulator.activateContract w $ TokenShopUser ts
        liftIO $ writeFile (cidFile w) $ show $ unContractInstanceId cid
        logString @(Builtin TokenShopContracts) $ "TokenShop user contract started for " ++ show w

    void $ liftIO getLine

    shutdown

waitForLast :: FromJSON a => ContractInstanceId -> Simulator.Simulation t a
waitForLast cid =
    flip Simulator.waitForState cid $ \json -> case fromJSON json of
        Success (Monoid.Last (Just x)) -> Just x
        _                       -> Nothing

handleTokenShopContract ::
    ( Member (Error PABError) effs
    , Member (LogMsg (PABMultiAgentMsg (Builtin TokenShopContracts))) effs
    )
    => ContractEffect (Builtin TokenShopContracts)
    ~> Eff effs
handleTokenShopContract = Builtin.handleBuiltin getSchema getContract where
  getSchema = \case
    TokenShopUser _ -> Builtin.endpointsToSchemas @(TSUseSchema .\\ BlockchainActions)
    TokenShopStart  -> Builtin.endpointsToSchemas @(TSStartSchema .\\ BlockchainActions)
  getContract = \case
    TokenShopUser ts -> SomeBuiltin $ useEndpoints ts
    TokenShopStart   -> SomeBuiltin startEndpoint

handlers :: SimulatorEffectHandlers (Builtin TokenShopContracts)
handlers =
    Simulator.mkSimulatorHandlers @(Builtin TokenShopContracts) []
    $ interpret handleTokenShopContract
