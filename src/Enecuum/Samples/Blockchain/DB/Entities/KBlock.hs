{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Samples.Blockchain.DB.Entities.KBlock where

import qualified Data.Aeson                           as A
import qualified Data.ByteString.Lazy                 as LBS
import           Enecuum.Prelude
import           Text.Printf                          (printf)

import           Enecuum.Samples.Blockchain.DB.Entities.Types (KBlockIdx)
import           Enecuum.Samples.Blockchain.DB.Model          (KBlocksDB)
import qualified Enecuum.Samples.Blockchain.Domain.KBlock     as D
import qualified Enecuum.Samples.Blockchain.Lens              as Lens
import qualified Enecuum.Core.Types                   as D


-- kBlocks (kBlock_idx|0 -> prev_hash, kBlock_idx|1 -> kBlock_data)
-- ------------------------------------------------------------
-- 0000000|0 AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=
-- 0000000|1 {time: 0, number:0, nonce: 0, solver: 1}

data KBlockPrevHashEntity
data KBlockEntity

instance D.DBModelEntity KBlocksDB KBlockPrevHashEntity
instance D.DBModelEntity KBlocksDB KBlockEntity

-- KBlockPrevHash entity

instance D.DBEntity KBlockPrevHashEntity where
    data DBKey   KBlockPrevHashEntity = KBlockPrevHashKey D.BlockNumber
        deriving (Show, Eq, Ord)
    data DBValue KBlockPrevHashEntity = KBlockPrevHashValue D.StringHash
        deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

instance D.ToDBKey KBlockPrevHashEntity D.BlockNumber where
    toDBKey = KBlockPrevHashKey

instance D.ToDBKey KBlockPrevHashEntity D.KBlock where
    toDBKey = KBlockPrevHashKey . D._number

instance D.ToDBValue KBlockPrevHashEntity D.KBlock where
    toDBValue kBlock = KBlockPrevHashValue $ kBlock ^. Lens.prevHash

-- TODO: this can be made by default
instance D.RawDBEntity KBlocksDB KBlockPrevHashEntity where
    toRawDBKey (KBlockPrevHashKey kBlockIdx) = encodeUtf8 $ toKBlockPrevHashEntityKeyBase kBlockIdx
    toRawDBValue = LBS.toStrict . A.encode
    fromRawDBValue = A.decode . LBS.fromStrict

-- KBlock entity

instance D.DBEntity KBlockEntity where
    data DBKey   KBlockEntity = KBlockKey D.BlockNumber
        deriving (Show, Eq, Ord)
    data DBValue KBlockEntity = KBlockValue D.BlockTime D.BlockNumber D.Nonce D.Solver
        deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

instance D.ToDBKey KBlockEntity D.BlockNumber where
    toDBKey = KBlockKey

instance D.ToDBKey KBlockEntity D.KBlock where
    toDBKey = KBlockKey . D._number

instance D.ToDBValue KBlockEntity D.KBlock where
    toDBValue (D.KBlock time _ number nonce solver) = KBlockValue time number nonce solver

instance D.RawDBEntity KBlocksDB KBlockEntity where
    toRawDBKey (KBlockKey kBlockIdx) = encodeUtf8 $ toKBlockEntityKeyBase kBlockIdx
    toRawDBValue = LBS.toStrict . A.encode
    fromRawDBValue = A.decode . LBS.fromStrict


toKBlockIdxBase :: KBlockIdx -> String
toKBlockIdxBase = printf "%07d"

toKBlockPrevHashEntityKeyBase :: KBlockIdx -> String
toKBlockPrevHashEntityKeyBase = (<> "0") . toKBlockIdxBase

toKBlockEntityKeyBase :: KBlockIdx -> String
toKBlockEntityKeyBase = (<> "1") . toKBlockIdxBase
