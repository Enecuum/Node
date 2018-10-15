{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Assets.Nodes.Messages where

import           Enecuum.Prelude
import qualified Enecuum.Domain                as D
import           Data.HGraph.StringHashable

data SuccessMsg = SuccessMsg
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

type WalletId = D.PublicKey

data GetLastKBlock = GetLastKBlock
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data GetWalletBalance = GetWalletBalance { walletId :: WalletId }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data GetGraphNode = GetGraphNode {hash :: StringHash}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ForeverChainGeneration = ForeverChainGeneration
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data NBlockPacketGeneration = NBlockPacketGeneration {number :: Int}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data WalletBalanceMsg = WalletBalanceMsg
  { walletId :: WalletId
  , balance  :: Integer
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
-------------------------------------------------------------------

data GetChainLengthRequest = GetChainLengthRequest
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype GetChainLengthResponse = GetChainLengthResponse { chainLength :: Integer }
  deriving (Show, Eq, Generic, Newtype, ToJSON, FromJSON)

data GetChainFromToRequest = GetChainFromToRequest { fromBlock :: Integer, toBlock :: Integer }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype GetChainFromToResponse = GetChainFromToResponse { blocks :: [D.KBlock] }
  deriving (Show, Eq, Generic, Newtype, ToJSON, FromJSON)

newtype GetMBlocksForKBlockRequest = GetMBlocksForKBlockRequest { kblock :: StringHash }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype GetMBlocksForKBlockResponse = GetMBlocksForKBlockResponse { mblocks :: [D.Microblock] }
  deriving (Show, Eq, Generic, Newtype, ToJSON, FromJSON)
