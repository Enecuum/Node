{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}


module Enecuum.Blockchain.Domain.Transaction where

import qualified Crypto.Hash.SHA256               as SHA
import qualified Data.ByteString.Base64           as Base64
import           Data.HGraph.StringHashable       (StringHash (..), StringHashable, toHash)
import qualified Data.Serialize                   as S
import           Data.UUID
import           Enecuum.Core.Crypto.Crypto
import           Enecuum.Blockchain.Domain.Types
import           Enecuum.Blockchain.Domain.UUID   ()
import qualified Enecuum.Core.Language            as L
import           Enecuum.Prelude                  hiding (show, unpack)

type OwnerPubKey = PublicKey
type OwnerPrivateKey = PrivateKey
type Receiver = PublicKey

data Transaction = Transaction
    { _owner     :: PublicKey
    , _receiver  :: PublicKey
    , _amount    :: Amount
    , _currency  :: Currency
    , _signature :: Signature
    , _uuid      :: UUID
    }
  deriving ( Generic, Show, Eq, Ord, Read, ToJSON, FromJSON, Serialize)

data TransactionForSign = TransactionForSign
    { _owner'    :: PublicKey
    , _receiver' :: PublicKey
    , _amount'   :: Amount
    , _currency' :: Currency
    }
  deriving ( Generic, Show, Eq, Ord, Read, ToJSON, FromJSON, Serialize)

instance StringHashable Transaction where
    toHash = StringHash . Base64.encode . SHA.hash . S.encode

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

showTransaction :: Transaction -> Text -> Text
showTransaction tx t =
    t <> ("\n    Tx: [" +|| ( showPublicKey $ _owner (tx :: Transaction)) ||+ "] -> [" +|| (showPublicKey $ _receiver (tx :: Transaction)) ||+
          "], amount: " +|| _amount (tx :: Transaction) ||+ ".")

showTxWithNewBalance :: Transaction -> Amount -> Amount -> Text
showTxWithNewBalance tx ownerBalance receiverBalance = showTx tx ||+
    ", owner balance: " +|| ownerBalance ||+
    ", receiver balance: " +|| receiverBalance ||+ "."

showTx :: Transaction -> Text
showTx tx =
  "    [" +|| ( showPublicKey $ _owner tx) ||+ "] -> [" +|| (showPublicKey $ _receiver (tx :: Transaction)) ||+
  "], amount: " +|| _amount (tx :: Transaction) ||+ ""
