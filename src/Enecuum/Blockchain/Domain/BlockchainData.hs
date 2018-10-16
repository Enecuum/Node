{-# LANGUAGE DeriveAnyClass #-}
module Enecuum.Blockchain.Domain.BlockchainData where 

import Enecuum.Prelude
import Enecuum.Blockchain.Domain.Graph (GraphVar)
import Enecuum.Blockchain.Domain.KBlock (KBlock)
import Enecuum.Framework.Domain.State (StateVar)
import Data.HGraph.StringHashable            (StringHash)
import Enecuum.Blockchain.Domain.Types (Amount)
import Enecuum.Blockchain.Domain.Crypto (PublicKey)

data BlockchainData = BlockchainData
    { _graph         :: GraphVar
    , _kBlockPending :: StateVar [KBlock]
    , _curNode       :: StateVar StringHash
    , _ledger        :: StateVar (Map PublicKey Amount)
    } 