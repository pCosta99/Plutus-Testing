{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE NumericUnderscores #-}

module Shop.TokenShopPAB where

import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import           GHC.Generics              (Generic)
import           Wallet.Emulator.Types     (Wallet (..), walletPubKey)
import qualified Ledger.Value                        as Value
import qualified Plutus.Contracts.Currency           as Currency
import           Data.Monoid                         (Last (..))
import           Plutus.Contract                     hiding (when)
import           Data.Text                 (Text, pack)
import           Ledger
import           Ledger.Constraints
import           Control.Monad              (when, forM_)

import Shop.Types

data TokenShopContracts =
      TokenShopStart
    | TokenShopUser TokenShop
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Pretty TokenShopContracts where
    pretty = viaShow

wallets :: [Wallet]
wallets = [Wallet i | i <- [1 .. 4]]

cidFile :: Wallet -> FilePath
cidFile w = "W" ++ show (getWallet w) ++ ".cid"
