{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Assets.Nodes.Messages where

import           Data.HGraph.StringHashable
import           Enecuum.Assets.Blockchain.Keys
import qualified Enecuum.Domain                 as D
import           Enecuum.Prelude
import Enecuum.Assets.Blockchain.Keys (WalletAlias)

-- | Common
data SuccessMsg = SuccessMsg
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype IsDead = IsDead StringHash
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


data GetNodeType = GetNodeType
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data NodeType = TypeGraphNode | TypeBN | TypePoA | TypePoW
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | BN, NN nodes
data Hello = Hello StringHash D.Address
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ConnectMapRequest = ConnectMapRequest
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | BN node
data ConnectRequest = ConnectRequest StringHash Word64
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype PreviousForMe = PreviousForMe StringHash
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype NextForMe = NextForMe StringHash
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data GetRoutingMessages = GetRoutingMessages
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Network messages
data Ping = Ping
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Pong = Pong
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Stop = Stop
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Client local
data CreateNodeId = CreateNodeId Password
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON )

data CreateWallet = CreateWallet Password
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON )

data CreateWalletWithAlias = CreateWalletWithAlias WalletAlias CreateWallet
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

data ShowMyWallets = ShowMyWallets
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

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

data NBlockPacketGeneration = NBlockPacketGeneration {number :: D.BlockNumber, timeGap :: Int}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ForeverChainGeneration = ForeverChainGeneration
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data GetPrevHash = GetPrevHash
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

newtype GetChainLengthResponse = GetChainLengthResponse { chainLength :: D.BlockNumber }
  deriving (Show, Eq, Generic, Newtype, ToJSON, FromJSON)

data GetChainFromToRequest = GetChainFromToRequest { fromBlock :: D.BlockNumber, toBlock :: D.BlockNumber }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype GetChainFromToResponse = GetChainFromToResponse { blocks :: [D.KBlock] }
  deriving (Show, Eq, Generic, Newtype, ToJSON, FromJSON)

newtype GetMBlocksForKBlockRequest = GetMBlocksForKBlockRequest { kblock :: StringHash }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype GetMBlocksForKBlockResponse = GetMBlocksForKBlockResponse { mblocks :: [D.Microblock] }
  deriving (Show, Eq, Generic, Newtype, ToJSON, FromJSON)

data Synchronize  = Synchronize D.Address
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Other graph node messages
data DumpToDB = DumpToDB
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data RestoreFromDB = RestoreFromDB
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
