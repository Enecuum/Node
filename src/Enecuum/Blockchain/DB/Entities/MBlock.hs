{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module Enecuum.Blockchain.DB.Entities.MBlock where

import           Enecuum.Prelude
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as LBS
import           Text.Printf          (printf)

import qualified Enecuum.Core.Types                    as D
import qualified Enecuum.Blockchain.Domain.KBlock      as D
import qualified Enecuum.Blockchain.Domain.Microblock  as D
import qualified Enecuum.Blockchain.Lens               as Lens
import           Enecuum.Blockchain.DB.Model           (MBlocksDB)
import           Enecuum.Blockchain.DB.Entities.Types  (KBlockIdx, MBlockIdx)
import           Enecuum.Blockchain.DB.Entities.KBlock (toKBlockIdxBase)


-- mBlocks (kBlock_idx|mBlock_idx -> mBlock_data)
-- --------------------------------------------------------------------
-- 0000001|001 {publisher: 3, signature: <signature>}
-- 0000001|002 {publisher: 5, signature: <signature>}
-- 0000002|001 {publisher: 1, signature: <signature>}

data MBlockEntity

instance D.DBModelEntity MBlocksDB MBlockEntity

instance D.DBEntity MBlockEntity where
    data DBKey MBlockEntity = MBlockKey ByteString
        deriving (Show, Eq, Ord)
    
    -- Publisher is temporarily not an index
    data DBValue MBlockEntity = MBlockValue
            { publisher :: D.PublicKey
            , signature :: D.Signature
            }    
        deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

instance D.ToDBKey MBlockEntity (KBlockIdx, MBlockIdx) where
    toDBKey (kBlockIdx, mBlockIdx) = MBlockKey . encodeUtf8 $ toKBlockIdxBase kBlockIdx <> toMBlockIdxBase mBlockIdx

instance D.ToDBValue MBlockEntity D.Microblock where
    toDBValue mBlock = MBlockValue (mBlock ^. Lens.publisher) (mBlock ^. Lens.signature)

instance D.RawDBEntity MBlocksDB MBlockEntity where
    toRawDBKey (MBlockKey k) = k
    toRawDBValue = LBS.toStrict . A.encode
    fromRawDBValue = A.decode . LBS.fromStrict

toMBlockIdxBase :: MBlockIdx -> String
toMBlockIdxBase = printf "%03d"
