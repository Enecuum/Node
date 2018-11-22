module Enecuum.Blockchain.Domain.BlockchainData where

import           Data.HGraph.StringHashable            (StringHash)
import qualified Data.Map                              as Map
import           Enecuum.Blockchain.Domain.Graph       (GraphVar)
import           Enecuum.Blockchain.Domain.KBlock      (BlockNumber, KBlock)
import           Enecuum.Blockchain.Domain.Transaction (Transaction)
import           Enecuum.Blockchain.Domain.Types       (Amount)
import           Enecuum.Core.Crypto.Crypto            (PublicKey)
import           Enecuum.Core.Types                    (StateVar)
import           Enecuum.Prelude

type WalletID = PublicKey
type Ledger = Map WalletID Amount
type TransactionPending = Map StringHash Transaction

-- Currently, pending allows only a single KBlock on each graph level (no forks)
type KBlockPending = Map.Map BlockNumber KBlock

data WindowedGraph = WindowedGraph
    { _graph        :: GraphVar
    , _windowSize   :: StateVar BlockNumber
    , _bottomKBlock :: StateVar StringHash
    , _topKBlock    :: StateVar StringHash
    }

data BlockchainData = BlockchainData
    { _windowedGraph      :: WindowedGraph
    , _kBlockPending      :: StateVar KBlockPending
    , _transactionPending :: StateVar TransactionPending
    , _ledger             :: StateVar Ledger
    }
