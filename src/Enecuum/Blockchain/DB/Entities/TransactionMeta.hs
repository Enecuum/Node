{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Blockchain.DB.Entities.TransactionMeta where

import qualified Data.Aeson                            as A
import qualified Data.ByteString.Lazy                  as LBS
import           Enecuum.Prelude
import           Text.Printf                           (printf)

import           Enecuum.Blockchain.DB.Entities.Types  (KBlockIdx, MBlockIdx, TransactionIdx)
import           Enecuum.Blockchain.DB.Model           (TransactionsDB)
import qualified Enecuum.Blockchain.Domain.KBlock      as D
import qualified Enecuum.Blockchain.Domain.Microblock  as D
import qualified Enecuum.Blockchain.Domain.Transaction as D
import qualified Enecuum.Blockchain.Lens               as Lens
import qualified Enecuum.Core.Types                    as D


-- txs_meta (tx_hash -> tx_meta)
-- tx_meta: (kBlock_idx, mBlock_idx, tx_idx, some_meta)
-- --------------------------------------------------------------------
-- <trans_hash> (1, 1, 1, "")
-- <trans_hash> (1, 1, 2, "")
-- <trans_hash> (2, 1, 1, "")

data TransactionMetaEntity

instance D.DBModelEntity TransactionsDB TransactionMetaEntity

instance D.DBEntity TransactionMetaEntity where
    data DBKey   TransactionMetaEntity = TransactionMetaKey D.StringHash
        deriving (Show, Eq, Ord)
    data DBValue TransactionMetaEntity = TransactionMetaValue (KBlockIdx, MBlockIdx, TransactionIdx)
        deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

instance D.ToDBKey TransactionMetaEntity D.Transaction where
    toDBKey = TransactionMetaKey . D.toHash

instance D.ToDBValue TransactionMetaEntity (KBlockIdx, MBlockIdx, TransactionIdx) where
    toDBValue = TransactionMetaValue

instance D.RawDBEntity TransactionsDB TransactionMetaEntity where
    toRawDBKey (TransactionMetaKey k) = D.fromStringHash k
    toRawDBValue = LBS.toStrict . A.encode
    fromRawDBValue = A.decode . LBS.fromStrict
