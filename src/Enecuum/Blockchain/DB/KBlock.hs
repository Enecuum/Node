{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module Enecuum.Blockchain.DB.KBlock where

import           Enecuum.Prelude
import           Text.Printf        (printf)

import qualified Enecuum.Core.Types as D


-- kBlocks (kBlock_idx|0 -> prev_hash, kBlock_idx|1 -> kBlock_data)
-- ------------------------------------------------------------
-- 0000000|0 AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=
-- 0000000|1 {number:0, nonce: 0, solver: 1}

data KBlocksDB
data KBlockPrevHashEntity
data KBlockEntity

instance D.DB KBlocksDB where
    getDbName = "kblocks"

instance D.DBEntity KBlockPrevHashEntity Integer where
    data DBKey   KBlockPrevHashEntity = KBlockPrevHashKey D.StringHash
        deriving (Show, Eq, Ord)
    data DBValue KBlockPrevHashEntity = KBlockPrevHashValue D.StringHash
        deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
    toDBKey   idx = KBlockPrevHashKey $ D.StringHash $ encodeUtf8 @String $ printf "%07d0" idx
    toDBValue _   = error "toDBValue not implemented for KBlockPrevHashEntity"

instance D.DBEntity KBlockEntity Integer where
    data DBKey   KBlockEntity = KBlockKey D.StringHash
        deriving (Show, Eq, Ord)
    data DBValue KBlockEntity = KBlockValue Integer Integer Integer D.StringHash
        deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
    toDBKey   idx = KBlockKey $ D.StringHash $ encodeUtf8 @String $ printf "%07d1" idx
    toDBValue _   = error "toDBValue not implemented for KBlockEntity"

instance D.GetRawDBEntity KBlockPrevHashEntity where
    getRawDBKey   (KBlockPrevHashKey k)   = D.fromStringHash k
    getRawDBValue (KBlockPrevHashValue _) = error "getRawDBValue not implemented for KBlockPrevHashEntity"

instance D.GetRawDBEntity KBlockEntity where
    getRawDBKey   (KBlockKey k)         = D.fromStringHash k
    getRawDBValue (KBlockValue _ _ _ _) = error "getRawDBValue not implemented for KBlockEntity"
