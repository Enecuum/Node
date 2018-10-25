{-# LANGUAGE DeriveAnyClass #-}
module Enecuum.Blockchain.Domain.BlockchainData where

import           Data.HGraph.StringHashable            (StringHash)
-- import qualified Data.Map                              as Map
import           Enecuum.Blockchain.Domain.Crypto      (PublicKey)
import           Enecuum.Blockchain.Domain.Graph       (GraphVar)
import           Enecuum.Blockchain.Domain.KBlock      (KBlock)
import           Enecuum.Blockchain.Domain.Transaction (Transaction)
import           Enecuum.Blockchain.Domain.Types       (Amount)
import           Enecuum.Framework.Domain.State        (StateVar)
import           Enecuum.Prelude

type WalletID = PublicKey
type Ledger = Map WalletID Amount
type TransactionPending = Map StringHash Transaction 

data BlockchainData = BlockchainData
    { _graph              :: GraphVar
    , _kBlockPending      :: StateVar [KBlock]
    , _transactionPending :: StateVar TransactionPending
    , _curNode            :: StateVar StringHash
    , _ledger             :: StateVar Ledger
    }
