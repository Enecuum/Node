{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module Enecuum.Blockchain.DB.Entities.KBlock where

import           Enecuum.Prelude
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as LBS
import           Text.Printf          (printf)

import qualified Enecuum.Core.Types as D
import qualified Enecuum.Blockchain.Domain.KBlock as D
import qualified Enecuum.Blockchain.Lens          as Lens
import           Enecuum.Blockchain.DB.Model (KBlocksDB)


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
    data DBKey   KBlockPrevHashEntity = KBlockPrevHashKey ByteString
        deriving (Show, Eq, Ord)
    data DBValue KBlockPrevHashEntity = KBlockPrevHashValue D.StringHash
        deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

instance D.ToDBKey KBlockPrevHashEntity D.BlockNumber where
    toDBKey = KBlockPrevHashKey . encodeUtf8 @String . printf "%07d0"

instance D.ToDBKey KBlockPrevHashEntity D.KBlock where
    toDBKey = KBlockPrevHashKey . encodeUtf8 @String . printf "%07d0" . D._number

instance D.ToDBValue KBlockPrevHashEntity D.KBlock where
    toDBValue kBlock = KBlockPrevHashValue $ kBlock ^. Lens.prevHash

-- TODO: this can be made by default
instance D.GetRawDBEntity KBlocksDB KBlockPrevHashEntity where
    getRawDBKey   (KBlockPrevHashKey k)   = k
    getRawDBValue (KBlockPrevHashValue k) = D.fromStringHash k

-- KBlock entity

instance D.DBEntity KBlockEntity where
    data DBKey   KBlockEntity = KBlockKey ByteString
        deriving (Show, Eq, Ord)
    data DBValue KBlockEntity = KBlockValue D.BlockTime D.BlockNumber D.Nonce D.Solver
        deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

instance D.ToDBKey KBlockEntity D.BlockNumber where
    toDBKey = KBlockKey . encodeUtf8 @String . printf "%07d1"

instance D.ToDBKey KBlockEntity D.KBlock where
    toDBKey = KBlockKey . encodeUtf8 @String . printf "%07d1" . D._number

instance D.ToDBValue KBlockEntity D.KBlock where
    toDBValue (D.KBlock time _ number nonce solver) = KBlockValue time number nonce solver

-- TODO: this can be made by default
instance D.GetRawDBEntity KBlocksDB KBlockEntity where
    getRawDBKey (KBlockKey k) = k
    getRawDBValue = LBS.toStrict . A.encode
