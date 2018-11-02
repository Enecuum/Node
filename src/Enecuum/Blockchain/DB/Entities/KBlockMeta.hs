{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module Enecuum.Blockchain.DB.Entities.KBlockMeta where

import           Enecuum.Prelude
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as LBS

import qualified Enecuum.Core.Types               as D
import qualified Enecuum.Blockchain.Domain.KBlock as D
import           Enecuum.Blockchain.DB.Model      (KBlocksMetaDB)

-- kBlocks_meta (kBlock_hash -> kBlock_meta)
-- -----------------------------------------------------------------
-- AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA= (0, "")
-- 4z9ADFAWehl6XGW2/N+2keOgNR921st3oPSVxv08hTY= (1, "")

data KBlockMetaEntity

instance D.DBModelEntity KBlocksMetaDB KBlockMetaEntity

instance D.DBEntity KBlockMetaEntity where
    data DBKey   KBlockMetaEntity = KBlockMetaKey D.DBKeyRaw
        deriving (Show, Eq, Ord)
    data DBValue KBlockMetaEntity = KBlockMetaValue Integer
        deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

instance D.ToDBKey KBlockMetaEntity D.KBlock where
    toDBKey = KBlockMetaKey . D.fromStringHash . D.toHash

instance D.ToDBValue KBlockMetaEntity D.KBlock where
    toDBValue kBlock = KBlockMetaValue $ D._number kBlock

instance D.ToDBKey KBlockMetaEntity D.StringHash where
    toDBKey = KBlockMetaKey . D.fromStringHash

instance D.GetRawDBEntity KBlocksMetaDB KBlockMetaEntity where
    getRawDBKey (KBlockMetaKey k) = k
    getRawDBValue v = LBS.toStrict $ A.encode v
