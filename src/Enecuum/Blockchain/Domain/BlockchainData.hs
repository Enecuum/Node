module Enecuum.Blockchain.Domain.BlockchainData where

import qualified Data.Map                              as Map
import           Data.HGraph.StringHashable            (StringHash)

import           Enecuum.Blockchain.Domain.Crypto      (PublicKey)
import           Enecuum.Blockchain.Domain.Graph       (GraphVar)
import           Enecuum.Blockchain.Domain.KBlock      (KBlock, BlockNumber)
import           Enecuum.Blockchain.Domain.Transaction (Transaction)
import           Enecuum.Blockchain.Domain.Types       (Amount)
import           Enecuum.Framework.Domain.State        (StateVar)
import           Enecuum.Prelude

type WalletID = PublicKey
type Ledger = Map WalletID Amount
type TransactionPending = Map StringHash Transaction

-- Currently, pending allows only a single KBlock on each graph level (no forks)
type KBlockPending = Map.Map BlockNumber KBlock

data BlockchainData = BlockchainData
    { _graph              :: GraphVar
    , _kBlockPending      :: StateVar KBlockPending
    , _transactionPending :: StateVar TransactionPending
    , _curNode            :: StateVar StringHash
    , _ledger             :: StateVar Ledger
    }
