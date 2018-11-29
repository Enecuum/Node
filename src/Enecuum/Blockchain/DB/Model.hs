module Enecuum.Blockchain.DB.Model where

import qualified Enecuum.Core.Types as D

data KBlocksDB
instance D.DB KBlocksDB where
    getDbName = "kblocks"

data KBlocksMetaDB
instance D.DB KBlocksMetaDB where
    getDbName = "kblocks_meta"

data MBlocksDB
instance D.DB MBlocksDB where
    getDbName = "mblocks"

data MBlocksMetaDB
instance D.DB MBlocksMetaDB where
    getDbName = "mblocks_meta"

data TransactionsDB
instance D.DB TransactionsDB where
    getDbName = "txs"

data TransactionsMetaDB
instance D.DB TransactionsMetaDB where
    getDbName = "txs_meta"

data DBModel = DBModel
    { _kBlocksDB          :: D.Storage KBlocksDB
    , _kBlocksMetaDB      :: D.Storage KBlocksMetaDB
    , _mBlocksDB          :: D.Storage MBlocksDB
    , _mBlocksMetaDB      :: D.Storage MBlocksMetaDB
    , _transactionsDB     :: D.Storage TransactionsDB
    , _transactionsMetaDB :: D.Storage TransactionsMetaDB
    }

