{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
module Enecuum.Blockchain.Domain.Transaction where

import           Enecuum.Blockchain.Domain.Crypto
import           Enecuum.Blockchain.Domain.Types
import qualified Enecuum.Core.Language            as L
import           Enecuum.Prelude
import           Data.Aeson.Extra (noLensPrefix)

type OwnerPubKey = PublicKey
type OwnerPrivateKey = PrivateKey
type Receiver = PublicKey

data Transaction = Transaction
    { _owner     :: PublicKey
    , _receiver  :: PublicKey
    , _amount    :: Amount
    , _currency  :: Currency
    , _signature :: Signature
    }
  deriving ( Generic, Show, Eq, Ord, Read, ToJSON, FromJSON, Serialize)

data TransactionForSign = TransactionForSign
    { _owner    :: PublicKey
    , _receiver :: PublicKey
    , _amount   :: Amount
    , _currency :: Currency
    }
  deriving ( Generic, Show, Eq, Ord, Read, ToJSON, FromJSON, Serialize)

data CLITransaction = CLITransaction
  { _owner     :: String
  , _receiver  :: String
  , _amount    :: Amount
  , _currency  :: Currency
  } deriving ( Generic, Show, Eq, Ord, Read, Serialize)  
instance ToJSON CLITransaction where toJSON = genericToJSON noLensPrefix
instance FromJSON CLITransaction where parseJSON = genericParseJSON noLensPrefix
cli1 = CLITransaction "me" "Alice" 15 ENQ

-- instance StringHashable Transaction where
--     toHash = StringHash . Base64.encode . SHA.hash . S.encode

transactionForSign :: Transaction -> TransactionForSign
transactionForSign (Transaction {..}) = TransactionForSign
  { _owner = _owner
  , _receiver = _receiver
  , _amount = _amount
  , _currency = _currency
  }

signTransaction :: (Monad m, L.ERandom m) => OwnerPubKey -> OwnerPrivateKey -> Receiver -> Amount -> Currency -> m Transaction
signTransaction owner ownerPriv receiver amount currency = do
  let tx = TransactionForSign
        { _owner = owner
        , _receiver = receiver
        , _amount = amount
        , _currency = currency
        }
  signature <- L.evalCoreCrypto $ L.sign ownerPriv tx
  pure $ Transaction
        { _owner = owner
        , _receiver = receiver
        , _amount = amount
        , _currency = currency
        , _signature = signature
        }

verifyTransaction :: Transaction -> Bool
verifyTransaction t@(Transaction {..}) = verifyEncodable _owner _signature (transactionForSign t)
