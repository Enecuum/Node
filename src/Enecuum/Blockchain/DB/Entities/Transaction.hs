{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module Enecuum.Blockchain.DB.Entities.Transaction where

import           Enecuum.Prelude
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as LBS
import           Text.Printf          (printf)

import qualified Enecuum.Core.Types as D
import qualified Enecuum.Blockchain.Domain.KBlock     as D
import qualified Enecuum.Blockchain.Domain.Microblock as D
import qualified Enecuum.Blockchain.Lens              as Lens
import           Enecuum.Blockchain.DB.Model          (TransactionsDB)
import           Enecuum.Blockchain.DB.Entities.Types (TransactionIdx)


-- txs (mBlock idx -> transaction data)
-- --------------------------------------------------------------------
-- 000000000 {owner: 1, receiver: 2, amount: 100: signature: <signature>}
-- 000000001 {owner: 2, receiver: 3, amount: 500: signature: <signature>}
-- 000000002 {owner: 1, receiver: 3, amount: 101: signature: <signature>}

data TransactionMetaEntity

instance D.DBModelEntity TransactionsDB TransactionMetaEntity

instance D.DBEntity TransactionMetaEntity where
    data DBKey   TransactionMetaEntity = TransactionMetaKey
        deriving (Show, Eq, Ord)
    data DBValue TransactionMetaEntity = TransactionMetaValue
        deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- instance D.ToDBKey TransactionMetaEntity D.BlockNumber where
--     toDBKey = KBlockPrevHashKey . encodeUtf8 @String . printf "%07d0"

-- instance D.ToDBValue TransactionMetaEntity D.Microblock where
--     toDBValue microblock = MicroblockMetaEntity ???

-- instance D.RawDBEntity TransactionsDB TransactionMetaEntity where
--     toRawDBKey (KBlockPrevHashKey k) = k
--     toRawDBValue = LBS.toStrict . A.encode
--     fromRawDBValue = A.decode . LBS.fromStrict
