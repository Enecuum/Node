{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Samples.Blockchain.DB.Entities.KBlockMeta where

import qualified Data.Aeson                       as A
import qualified Data.ByteString.Lazy             as LBS
import           Enecuum.Prelude

import           Enecuum.Samples.Blockchain.DB.Model      (KBlocksMetaDB)
import qualified Enecuum.Samples.Blockchain.Domain.KBlock as D
import qualified Enecuum.Core.Types               as D

-- kBlocks_meta (kBlock_hash -> kBlock_meta)
-- -----------------------------------------------------------------
-- AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA= (0, "")
-- 4z9ADFAWehl6XGW2/N+2keOgNR921st3oPSVxv08hTY= (1, "")

data KBlockMetaEntity

instance D.DBModelEntity KBlocksMetaDB KBlockMetaEntity

instance D.DBEntity KBlockMetaEntity where
    data DBKey   KBlockMetaEntity = KBlockMetaKey D.StringHash
        deriving (Show, Eq, Ord)
    data DBValue KBlockMetaEntity = KBlockMetaValue D.BlockNumber
        deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

instance D.ToDBKey KBlockMetaEntity D.KBlock where
    toDBKey = KBlockMetaKey . D._prevHash

instance D.ToDBValue KBlockMetaEntity D.KBlock where
    toDBValue kBlock = KBlockMetaValue $ D._number kBlock

instance D.ToDBKey KBlockMetaEntity D.StringHash where
    toDBKey = KBlockMetaKey

instance D.RawDBEntity KBlocksMetaDB KBlockMetaEntity where
    toRawDBKey (KBlockMetaKey k) = D.fromStringHash k
    toRawDBValue = LBS.toStrict . A.encode
    fromRawDBValue = A.decode . LBS.fromStrict
