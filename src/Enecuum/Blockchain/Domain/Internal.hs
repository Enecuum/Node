{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
module Enecuum.Blockchain.Domain.Internal (signTransaction, transactionForSign, microblockForSign, signMicroblock) where

import           Data.HGraph.StringHashable            (StringHash (..), StringHashable, toHash)
import           Data.UUID
import           Enecuum.Blockchain.Domain.Microblock
import           Enecuum.Blockchain.Domain.Transaction
import           Enecuum.Blockchain.Domain.Types
import           Enecuum.Blockchain.Domain.UUID        ()
import           Enecuum.Core.Crypto.Crypto
import qualified Enecuum.Core.Language                 as L
import           Enecuum.Prelude

data TransactionForSign = TransactionForSign
    { _owner'    :: PublicKey
    , _receiver' :: PublicKey
    , _amount'   :: Amount
    , _currency' :: Currency
    }
  deriving ( Generic, Show, Eq, Ord, Read, ToJSON, FromJSON, Serialize)

transactionForSign :: Transaction -> TransactionForSign
transactionForSign (Transaction {..}) = TransactionForSign
  { _owner' = _owner
  , _receiver' = _receiver
  , _amount' = _amount
  , _currency' = _currency
  }

signTransaction :: (Monad m, L.ERandom m) => OwnerPubKey -> OwnerPrivateKey -> Receiver -> Amount -> Currency -> UUID -> m Transaction
signTransaction owner ownerPriv receiver amount currency uuid = do
  let tx = TransactionForSign
        { _owner' = owner
        , _receiver' = receiver
        , _amount' = amount
        , _currency' = currency
        }
  signature <- L.evalCoreCrypto $ L.sign ownerPriv tx
  pure $ Transaction
        { _owner = owner
        , _receiver = receiver
        , _amount = amount
        , _currency = currency
        , _signature = signature
        , _uuid = uuid
        }

data MicroblockForSign = MicroblockForSign
    { _keyBlock     :: StringHash
    , _transactions :: [Transaction]
    , _publisher    :: PublicKey
    }
    deriving (Eq, Generic, Ord, Read, Show, ToJSON, FromJSON, Serialize)

microblockForSign :: Microblock -> MicroblockForSign
microblockForSign Microblock {..} = MicroblockForSign
    { _keyBlock = _keyBlock
    , _transactions = _transactions
    , _publisher = _publisher
    }

signMicroblock :: (Monad m, L.ERandom m) => StringHash -> [Transaction] -> PublicKey -> PrivateKey -> m Microblock
signMicroblock hashofKeyBlock tx publisherPubKey publisherPrivKey = do
    let mb = MicroblockForSign
            { _keyBlock = hashofKeyBlock
            , _transactions = tx
            , _publisher = publisherPubKey
            }
    signature <- L.evalCoreCrypto $ L.sign publisherPrivKey mb
    pure $ Microblock
            { _keyBlock = hashofKeyBlock
            , _transactions = tx
            , _publisher = publisherPubKey
            , _signature = signature
            }
