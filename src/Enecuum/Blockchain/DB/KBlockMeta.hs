{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module Enecuum.Blockchain.DB.KBlockMeta where

import           Enecuum.Prelude
import qualified Data.Aeson                as A
import qualified Data.ByteString.Lazy      as LBS

import qualified Enecuum.Core.Types        as D
import qualified Enecuum.Blockchain.Domain as D
import qualified Enecuum.Blockchain.Lens   as Lens

-- kBlocks_meta (kBlock_hash -> kBlock_meta)
-- -----------------------------------------------------------------
-- 4z9ADFAWehl6XGW2/N+2keOgNR921st3oPSVxv08hTY= (0, "")

data KBlocksMetaDB
data KBlockMetaEntity

instance D.DB KBlocksMetaDB where
    getDbName = "kblocks_meta"

instance D.DBEntity KBlockMetaEntity D.KBlock where
    data DBKey   KBlockMetaEntity = KBlockMetaKey D.DBKeyRaw
        deriving (Show, Eq, Ord)
    data DBValue KBlockMetaEntity = KBlockMetaValue Integer
        deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
    toDBKey          = KBlockMetaKey . D.fromStringHash . D.toHash
    toDBValue kBlock = KBlockMetaValue $ kBlock ^. Lens.number

instance D.GetRawDBEntity KBlockMetaEntity where
    getRawDBKey (KBlockMetaKey k) = k
    getRawDBValue v = LBS.toStrict $ A.encode v
