{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Shop.OffChain
    ( TSStartSchema
    , TSUseSchema
    , startEndpoint
    , useEndpoints
    ) where

import           Control.Monad                hiding (fmap)
import           Data.Monoid                  (Last (..))
import           Data.Text                    (Text, pack)
import           Plutus.Contract              as Contract hiding (when)
import           Plutus.Contract.StateMachine
import qualified Plutus.Contracts.Currency    as C
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)
import           Ledger                       hiding (singleton)
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Value
import           Prelude                      (Show (..), uncurry, String)
import qualified Data.Map as Map

import Shop.Types
import Shop.OnChain

{-# INLINABLE tsStateMachine #-}
tsStateMachine :: TokenShop -> StateMachine (Maybe Integer) TSRedeemer
tsStateMachine ts = mkStateMachine (Just $ tsNFT ts) (transition ts) isNothing

{-# INLINABLE mkTSValidator #-}
mkTSValidator :: TokenShop -> Maybe Integer -> TSRedeemer -> ScriptContext -> Bool
mkTSValidator = mkValidator . tsStateMachine

type TS = StateMachine (Maybe Integer) TSRedeemer

tsInst :: TokenShop -> Scripts.ScriptInstance TS
tsInst ts = Scripts.validator @TS
    ($$(PlutusTx.compile [|| mkTSValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode ts)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @(Maybe Integer) @TSRedeemer

tsValidator :: TokenShop -> Validator
tsValidator = Scripts.validatorScript . tsInst

tsAddress :: TokenShop -> Ledger.Address
tsAddress = scriptAddress . tsValidator

tsClient :: TokenShop -> StateMachineClient (Maybe Integer) TSRedeemer
tsClient ts = mkStateMachineClient $ StateMachineInstance (tsStateMachine ts) (tsInst ts)

mapErrorC :: Contract w s C.CurrencyError a -> Contract w s Text a
mapErrorC = mapError $ pack . show

mapErrorSM :: Contract w s SMContractError a -> Contract w s Text a
mapErrorSM = mapError $ pack . show

startTS :: HasBlockchainActions s => Contract (Last TokenShop) s Text ()
startTS = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    csNFT  <- C.currencySymbol <$> mapErrorC (C.forgeContract pkh [(nftName, 1)])
    csBB <- C.currencySymbol <$> mapErrorC (C.forgeContract pkh [(tsTokenName, 1)])
    let ts = TokenShop
            { tsSeller = pkh
            , tsToken  = AssetClass (csBB, tsTokenName)
            , tsNFT    = AssetClass (csNFT, nftName)
            }
        client = tsClient ts
    void $ mapErrorSM $ runInitialise client (Just 0) mempty
    tell $ Last $ Just ts
    logInfo $ "started token sale " ++ show ts
    return ()

setPrice :: HasBlockchainActions s => TokenShop -> Integer -> Contract w s Text ()
setPrice ts p = void $ mapErrorSM $ runStep (tsClient ts) $ SetPrice p

addTokens :: HasBlockchainActions s => TokenShop -> Integer -> Contract w s Text ()
addTokens ts n = void (mapErrorSM $ runStep (tsClient ts) $ AddTokens n)

buyTokens :: HasBlockchainActions s => TokenShop -> Integer -> Contract w s Text ()
buyTokens ts n = void $ mapErrorSM $ runStep (tsClient ts) $ BuyTokens n

withdraw :: HasBlockchainActions s => TokenShop -> Integer -> Integer -> Contract w s Text ()
withdraw ts n l = void $ mapErrorSM $ runStep (tsClient ts) $ Withdraw n l

close :: HasBlockchainActions s => TokenShop -> Contract w s Text ()
close ts = void $ mapErrorSM $ runStep (tsClient ts) Close

-- | Gets the caller's funds.
funds :: HasBlockchainActions s => Contract (Last Value) s Text ()
funds = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    os  <- map snd . Map.toList <$> utxoAt (pubKeyHashAddress pkh)
    let v = mconcat [txOutValue $ txOutTxOut o | o <- os]
    tell $ Last $ Just v
    return ()

type TSStartSchema = BlockchainActions
    .\/ Endpoint "start"      (CurrencySymbol, TokenName)
type TSUseSchema = BlockchainActions
    .\/ Endpoint "funds" ()
    .\/ Endpoint "set price"  Integer
    .\/ Endpoint "add tokens" Integer
    .\/ Endpoint "buy tokens" Integer
    .\/ Endpoint "withdraw"   (Integer, Integer)
    .\/ Endpoint "close"      ()

startEndpoint :: Contract (Last TokenShop) TSStartSchema Text ()
startEndpoint = startTS

useEndpoints :: TokenShop -> Contract (Last Value) TSUseSchema Text ()
useEndpoints ts = (funds' `select` setPrice' `select` addTokens' `select` buyTokens' `select` withdraw' `select` close') >> useEndpoints ts
  where
    funds'     = handleError logError $ endpoint @"funds" >> funds
    setPrice'  = handleError logError $ endpoint @"set price"  >>= setPrice ts
    addTokens' = handleError logError $ endpoint @"add tokens" >>= addTokens ts
    buyTokens' = handleError logError $ endpoint @"buy tokens" >>= buyTokens ts
    withdraw'  = handleError logError $ endpoint @"withdraw"   >>= uncurry (withdraw ts)
    close'     = handleError logError $ endpoint @"close"      >>  close ts

