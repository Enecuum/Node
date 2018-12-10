{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}


module Enecuum.Samples.Blockchain.Domain.Transaction where

import qualified Crypto.Hash.SHA256                 as SHA
import qualified Data.ByteString.Base64             as Base64
import           Data.HGraph.StringHashable         (StringHash (..), StringHashable, toHash)
import qualified Data.Serialize                     as S
import           Data.UUID
import           Enecuum.Samples.Blockchain.Domain.Types
import           Enecuum.Samples.Blockchain.Domain.UUID     ()
import           Enecuum.Core.Crypto.Crypto
import           Enecuum.Prelude                    hiding (show, unpack)

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

instance StringHashable Transaction where
    toHash = StringHash . Base64.encode . SHA.hash . S.encode

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
