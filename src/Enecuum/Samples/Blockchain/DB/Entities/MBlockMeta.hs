{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Samples.Blockchain.DB.Entities.MBlockMeta where

import qualified Data.Aeson                           as A
import qualified Data.ByteString.Lazy                 as LBS
import           Enecuum.Prelude
import           Text.Printf                          (printf)

import           Enecuum.Samples.Blockchain.DB.Entities.Types (KBlockIdx, MBlockIdx)
import           Enecuum.Samples.Blockchain.DB.Model          (MBlocksMetaDB)
import qualified Enecuum.Samples.Blockchain.Domain.KBlock     as D
import qualified Enecuum.Samples.Blockchain.Domain.Microblock as D
import qualified Enecuum.Samples.Blockchain.Lens              as Lens
import qualified Enecuum.Core.Types                   as D


-- mBlocks_meta (mBlock_hash -> mBlock_meta)
-- mBlockMeta: (kBlock_idx, mBlock_idx, some_meta)
-- --------------------------------------------------------------------
-- <mblock_hash> (1, 1, "")
-- <mblock_hash> (1, 2, "")
-- <mblock_hash> (2, 1, "")

data MBlockMetaEntity

instance D.DBModelEntity MBlocksMetaDB MBlockMetaEntity

instance D.DBEntity MBlockMetaEntity where
    data DBKey   MBlockMetaEntity = MBlockMetaKey D.StringHash
        deriving (Show, Eq, Ord)
    data DBValue MBlockMetaEntity = MBlockMetaValue (KBlockIdx, MBlockIdx)
        deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

instance D.ToDBKey MBlockMetaEntity D.Microblock where
    toDBKey = MBlockMetaKey . D.toHash

instance D.ToDBValue MBlockMetaEntity (KBlockIdx, MBlockIdx) where
    toDBValue = MBlockMetaValue

instance D.RawDBEntity MBlocksMetaDB MBlockMetaEntity where
    toRawDBKey (MBlockMetaKey k) = D.fromStringHash k
    toRawDBValue = LBS.toStrict . A.encode
    fromRawDBValue = A.decode . LBS.fromStrict
