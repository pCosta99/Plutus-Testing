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

module Shop.OnChain where

import Control.Monad (guard)
import           Plutus.Contract.StateMachine
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)
import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import           Ledger.Value
import           Prelude                      (Semigroup (..))

import Shop.Types

{-# INLINABLE nft #-}
nft :: TokenShop ->Integer -> Value
nft ts = assetClassValue (tsNFT ts)

{-# INLINABLE f #-}
f :: Bool  -- ^ A condition that determines if the transition should happen
  ->Maybe Integer -- ^ The datum
  ->TxConstraints Void Void -- ^ The constraints that the transition needs to fullfill
  ->(Integer ->State (Maybe Integer)) -- ^ A function that produces a state, to be used when the datum contains a value
  ->Maybe (TxConstraints Void Void, State (Maybe Integer)) -- ^ The desired result
f pred dat tx st = (\p -> (tx, st p)) <$> (guard pred >> dat)

{-# INLINABLE setPriceTransition #-}
setPriceTransition :: TokenShop ->Value ->Maybe Integer ->Integer ->Maybe (TxConstraints Void Void, State (Maybe Integer))
setPriceTransition ts v dat p = f (p >= 0) dat (Constraints.mustBeSignedBy (tsSeller ts)) (const $ State (Just p) $ v <> (nft ts) (negate 1))

{-# INLINABLE addTokensTransition #-}
addTokensTransition :: TokenShop ->Value ->Maybe Integer ->Integer ->Maybe (TxConstraints Void Void, State (Maybe Integer))
addTokensTransition ts v dat n = f (n > 0) dat mempty (\p ->(State (Just p) $ v <> (nft ts) (negate 1) <> assetClassValue (tsToken ts) n))

{-# INLINABLE buyTokensTransition #-}
buyTokensTransition :: TokenShop ->Value ->Maybe Integer -> Integer ->Maybe (TxConstraints Void Void, State (Maybe Integer))
buyTokensTransition ts v dat n = f (n > 0) dat mempty (\p ->State (Just p) $ v <> (nft ts) (negate 1) <> assetClassValue (tsToken ts) (negate n) <> lovelaceValueOf (n * p))

{-# INLINABLE withdrawTransition #-}
withdrawTransition :: TokenShop ->Value ->Maybe Integer ->Integer ->Integer ->Maybe (TxConstraints Void Void, State (Maybe Integer))
withdrawTransition ts v dat n l = f (n >= 0 && l >= 0) dat (Constraints.mustBeSignedBy (tsSeller ts)) (\p ->State (Just p) $ v <> (nft ts) (negate 1) <> assetClassValue (tsToken ts) (negate n) <> lovelaceValueOf (negate l))

{-# INLINABLE closeTransition #-}
closeTransition :: TokenShop ->Maybe Integer ->Maybe (TxConstraints Void Void, State (Maybe Integer))
closeTransition ts dat = f True dat (Constraints.mustBeSignedBy (tsSeller ts)) (const $ State Nothing mempty)

{-# INLINABLE transition #-}
transition :: TokenShop ->State (Maybe Integer) -> TSRedeemer -> Maybe (TxConstraints Void Void, State (Maybe Integer))
transition ts s (SetPrice p) = setPriceTransition ts (stateValue s) (stateData s) p
transition ts s (AddTokens n) = addTokensTransition ts (stateValue s) (stateData s) n
transition ts s (BuyTokens n) = buyTokensTransition ts (stateValue s) (stateData s) n
transition ts s (Withdraw n l) = withdrawTransition ts (stateValue s) (stateData s) n l
transition ts s Close = closeTransition ts (stateData s)
