{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Samples.Blockchain.DB.Entities.Transaction where

import qualified Data.Aeson                            as A
import qualified Data.ByteString.Lazy                  as LBS
import           Enecuum.Prelude
import           Text.Printf                           (printf)

import           Enecuum.Samples.Blockchain.DB.Entities.KBlock (toKBlockIdxBase)
import           Enecuum.Samples.Blockchain.DB.Entities.MBlock (toMBlockIdxBase)
import           Enecuum.Samples.Blockchain.DB.Entities.Types  (KBlockIdx, MBlockIdx, TransactionIdx)
import           Enecuum.Samples.Blockchain.DB.Model           (TransactionsDB)
import qualified Enecuum.Samples.Blockchain.Domain.KBlock      as D
import qualified Enecuum.Samples.Blockchain.Domain.Microblock  as D
import qualified Enecuum.Samples.Blockchain.Domain.Transaction as D
import qualified Enecuum.Samples.Blockchain.Domain.Types       as D
import qualified Enecuum.Samples.Blockchain.Lens               as Lens
import qualified Enecuum.Core.Types                    as D


-- txs (kBlock_idx|mBlock_idx|tx_idx -> tx_data)
-- --------------------------------------------------------------------
-- 0000001|001|001 {owner: 1, receiver: 2, amount: 100: signature: <signature>}
-- 0000001|001|002 {owner: 2, receiver: 3, amount: 500: signature: <signature>}
-- 0000002|001|001 {owner: 1, receiver: 3, amount: 101: signature: <signature>}

data TransactionEntity

instance D.DBModelEntity TransactionsDB TransactionEntity

instance D.DBEntity TransactionEntity where
    data DBKey   TransactionEntity = TransactionKey (KBlockIdx, MBlockIdx, TransactionIdx)
        deriving (Show, Eq, Ord)
    data DBValue TransactionEntity = TransactionValue
            { owner     :: D.PublicKey  -- Temporarily not an index
            , receiver  :: D.PublicKey  -- Temporarily not an index
            , amount    :: D.Amount
            , signature :: D.Signature  -- Temporarily not an index
            , uuid      :: D.UUID
            }
        deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

instance D.ToDBKey TransactionEntity (KBlockIdx, MBlockIdx, TransactionIdx) where
    toDBKey = TransactionKey

instance D.ToDBValue TransactionEntity D.Transaction where
    toDBValue tx = TransactionValue
        { owner     = tx ^. Lens.owner
        , receiver  = tx ^. Lens.receiver
        , amount    = tx ^. Lens.amount
        , signature = tx ^. Lens.signature
        , uuid      = tx ^. Lens.uuid
        }

instance D.RawDBEntity TransactionsDB TransactionEntity where
    toRawDBKey (TransactionKey (kBlockIdx, mBlockIdx, transactionIdx)) =
        encodeUtf8
            $  toKBlockIdxBase kBlockIdx
            <> toMBlockIdxBase mBlockIdx
            <> toTransactionIdxBase transactionIdx
    toRawDBValue   = LBS.toStrict . A.encode
    fromRawDBValue = A.decode . LBS.fromStrict

toTransactionIdxBase :: TransactionIdx -> String
toTransactionIdxBase = printf "%03d"

fromDBTransaction
    :: D.DBValue TransactionEntity
    -> D.Transaction
fromDBTransaction transValue = D.Transaction
    { D._owner     = owner     transValue
    , D._receiver  = receiver  transValue
    , D._amount    = amount    transValue
    , D._currency  = D.ENQ                  -- TODO: only ENQ currency is supported.
    , D._signature = signature transValue
    , D._uuid      = uuid      transValue
    }
