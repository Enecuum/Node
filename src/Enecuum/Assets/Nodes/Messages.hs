{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Assets.Nodes.Messages where

import           Enecuum.Prelude
import qualified Enecuum.Domain                as D


newtype AcceptKeyBlockRequest  = AcceptKeyBlockRequest { kBlock :: D.KBlock }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AcceptKeyBlockResponse = AcceptKeyBlockResponse { accepted :: Bool }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

type WalletId = Int

data GetBalanceOfWalletRequest = GetBalanceOfWalletRequest { walletId :: WalletId }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

data GetBalanceOfWalletResponse = GetBalanceOfWalletResponse
    { walletId :: WalletId
    , balance  :: Int
    }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)