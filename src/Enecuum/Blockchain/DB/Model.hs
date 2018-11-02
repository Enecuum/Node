module Enecuum.Blockchain.DB.Model where

import           Enecuum.Prelude

import qualified Enecuum.Core.Types as D

data KBlocksDB
instance D.DB KBlocksDB where
    getDbName = "kblocks"

data KBlocksMetaDB
instance D.DB KBlocksMetaDB where
    getDbName = "kblocks_meta"

data DBModel = DBModel
    { _kBlocksDB     :: D.Storage KBlocksDB
    , _kBlocksMetaDB :: D.Storage KBlocksMetaDB
    }

dbExt :: FilePath
dbExt = ".db"
