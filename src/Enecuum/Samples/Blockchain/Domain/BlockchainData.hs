{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Samples.Blockchain.Domain.BlockchainData where

import           Data.HGraph.StringHashable            (StringHash)
import qualified Data.Map                              as Map
import           Enecuum.Samples.Blockchain.Domain.Graph       (GraphVar)
import           Enecuum.Samples.Blockchain.Domain.KBlock      (BlockNumber, KBlock)
import           Enecuum.Samples.Blockchain.Domain.Transaction (Transaction)
import           Enecuum.Samples.Blockchain.Domain.Types       (Amount)
import           Enecuum.Core.Crypto.Crypto            (PublicKey)
import           Enecuum.Core.Types                    (StateVar)
import           Enecuum.Prelude

type WalletID = PublicKey
type Ledger = Map WalletID Amount
type TransactionPending = Map StringHash Transaction

-- Currently, pending allows only a single KBlock on each graph level (no forks)
type KBlockPending = Map.Map BlockNumber KBlock

data WindowedGraph = WindowedGraph
    { _graph            :: GraphVar
    , _bottomKBlockHash :: StateVar StringHash
    , _topKBlockHash    :: StateVar StringHash
    }

data BlockchainData = BlockchainData
    { _windowedGraph      :: WindowedGraph
    , _kBlockPending      :: StateVar KBlockPending
    , _transactionPending :: StateVar TransactionPending
    , _ledger             :: StateVar Ledger
    }
