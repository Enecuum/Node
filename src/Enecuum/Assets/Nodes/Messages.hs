{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module Enecuum.Assets.Nodes.Messages where

import           Enecuum.Prelude
import qualified Enecuum.Domain                as D
import           Data.HGraph.StringHashable

-- | Common
data SuccessMsg = SuccessMsg
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


-- | Routing messages
-- | Client
data SendTo = SendTo StringHash Int Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | BN, NN nodes
data Hello = Hello StringHash D.Address
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ConnectMapRequest = ConnectMapRequest
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | BN node
data ConnectRequest = ConnectRequest StringHash Integer
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype ConnectRequestPrevious = ConnectRequestPrevious StringHash
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype NextForMe = NextForMe StringHash
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | NN node  
newtype NextForYou = NextForYou D.Address
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ConnectResponse = ConnectResponse StringHash D.Address
  deriving (Show, Eq, Generic, ToJSON, FromJSON)  


-- | Network messages
data Ping = Ping
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Pong = Pong
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Stop = Stop
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

  
-- | client - graph node interaction
newtype CreateTransaction = CreateTransaction D.Transaction
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

type WalletId = D.PublicKey

data WalletBalanceMsg = WalletBalanceMsg
  { walletId :: WalletId
  , balance  :: Integer
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype GetWalletBalance = GetWalletBalance { walletId :: WalletId }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


-- | client - PoW interaction
data GetKBlockPending = GetKBlockPending
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype NBlockPacketGeneration = NBlockPacketGeneration {number :: Int}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ForeverChainGeneration = ForeverChainGeneration
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


-- | client - PoA interaction
data GetTransactionPending = GetTransactionPending
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


-- | Graph nodes sync interaction
data GetLastKBlock = GetLastKBlock
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype GetGraphNode = GetGraphNode {hash :: StringHash}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

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
