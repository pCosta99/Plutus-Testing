{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Shop.Types
    ( TokenShop (..)
    , TSRedeemer (..)
    , nftName
    , tsTokenName
    ) where

import           Data.Aeson                   (FromJSON, ToJSON)
import           GHC.Generics                 (Generic)
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)
import           Ledger                       hiding (singleton)
import           Ledger.Value
import           Prelude                      (Show (..))
import qualified Prelude

data TokenShop = TokenShop
    { tsSeller :: !PubKeyHash
    , tsToken  :: !AssetClass
    , tsNFT    :: !AssetClass
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''TokenShop

tsTokenName :: TokenName
tsTokenName = "BB"

data TSRedeemer =
      SetPrice Integer
    | AddTokens Integer
    | BuyTokens Integer
    | Withdraw Integer Integer
    | Close
    deriving (Show, Prelude.Eq)

PlutusTx.unstableMakeIsData ''TSRedeemer

nftName :: TokenName
nftName = "NFT"
