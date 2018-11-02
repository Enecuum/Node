{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module Enecuum.Blockchain.DB.Entities.KBlock where

import           Enecuum.Prelude
import           Text.Printf        (printf)

import qualified Enecuum.Core.Types as D
import qualified Enecuum.Blockchain.Domain.KBlock as D
import           Enecuum.Blockchain.DB.Model      (KBlocksDB)


-- kBlocks (kBlock_idx|0 -> prev_hash, kBlock_idx|1 -> kBlock_data)
-- ------------------------------------------------------------
-- 0000000|0 AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=
-- 0000000|1 {number:0, nonce: 0, solver: 1}

data KBlockPrevHashEntity
data KBlockEntity

instance D.DBModelEntity KBlocksDB KBlockPrevHashEntity
instance D.DBModelEntity KBlocksDB KBlockEntity

-- KBlockPrevHash entity

instance D.DBEntity KBlockPrevHashEntity where
    data DBKey   KBlockPrevHashEntity = KBlockPrevHashKey D.StringHash
        deriving (Show, Eq, Ord)
    data DBValue KBlockPrevHashEntity = KBlockPrevHashValue D.StringHash
        deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

instance D.GetDBKey KBlockPrevHashEntity D.Number where
    toDBKey idx = KBlockPrevHashKey $ D.StringHash $ encodeUtf8 @String $ printf "%07d0" idx

-- instance D.GetDBValue KBlockPrevHashEntity D.KBlock where
--     toDBValue _ = error "toDBValue not implemented for KBlockPrevHashEntity"

-- TODO: this can be made by default
instance D.GetRawDBEntity KBlocksDB KBlockPrevHashEntity where
    getRawDBKey   (KBlockPrevHashKey k)   = D.fromStringHash k
    getRawDBValue (KBlockPrevHashValue _) = error "getRawDBValue not implemented for KBlockPrevHashEntity"

-- KBlock entity

instance D.DBEntity KBlockEntity where
    data DBKey   KBlockEntity = KBlockKey D.StringHash
        deriving (Show, Eq, Ord)
    data DBValue KBlockEntity = KBlockValue D.Time' D.Number D.Nonce D.Solver
        deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

instance D.GetDBKey KBlockEntity D.Number where
    toDBKey   idx = KBlockKey $ D.StringHash $ encodeUtf8 @String $ printf "%07d1" idx

-- instance D.GetDBValue KBlockEntity D.KBlock where
--     toDBValue _   = error "toDBValue not implemented for KBlockEntity"

-- TODO: this can be made by default
instance D.GetRawDBEntity KBlocksDB KBlockEntity where
    getRawDBKey   (KBlockKey k)         = D.fromStringHash k
    getRawDBValue (KBlockValue _ _ _ _) = error "getRawDBValue not implemented for KBlockEntity"
