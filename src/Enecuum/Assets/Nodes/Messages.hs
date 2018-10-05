{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Assets.Nodes.Messages where

import           Enecuum.Prelude
import qualified Enecuum.Domain                as D
import           Data.HGraph.StringHashable

newtype AcceptKeyBlockRequest  = AcceptKeyBlockRequest { kBlock :: D.KBlock }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AcceptKeyBlockResponse = AcceptKeyBlockResponse { accepted :: Bool }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype AcceptMicroblockRequest  = AcceptMicroblockRequest { microblock :: D.Microblock }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AcceptMicroblockResponse = AcceptMicroblockResponse { accepted :: Bool }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

type WalletId = Int

data GetLastKBlockRequest = GetLastKBlockRequest
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data GetLastKBlockResponse = GetLastKBlockResponse {kBlock :: D.KBlock}

--data GetBlockRequest = GetBlockRequest {hash :: StringHash}


data GetBalanceOfWalletRequest = GetBalanceOfWalletRequest { walletId :: WalletId }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

data GetBalanceOfWalletResponse = GetBalanceOfWalletResponse
    { walletId :: WalletId
    , balance  :: Int
    }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)