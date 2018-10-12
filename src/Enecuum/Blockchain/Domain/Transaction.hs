{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Enecuum.Blockchain.Domain.Transaction where

import           Enecuum.Prelude

-- import qualified Crypto.Hash.SHA256               as SHA
-- import qualified Data.ByteString.Base64           as Base64
-- import           Data.HGraph.StringHashable       (StringHash (..), StringHashable, toHash)
import           Enecuum.Blockchain.Domain.Crypto
import           Enecuum.Blockchain.Domain.Types

-- This data structure is for tests of graph incorporation only.
-- Please, replace it by actual blockchain data.

data Transaction = Transaction
    { _owner     :: PublicKey
    , _receiver  :: PublicKey
    , _amount    :: Amount
    , _currency  :: Currency
    , _signature :: Signature
    }
  deriving ( Generic, Show, Eq, Ord, Read, ToJSON, FromJSON, Serialize)

data TransactionForSign = TransactionForSign
    { _owner     :: PublicKey
    , _receiver  :: PublicKey
    , _amount    :: Amount
    , _currency  :: Currency
    }
  deriving ( Generic, Show, Eq, Ord, Read, ToJSON, FromJSON, Serialize)

-- instance StringHashable Transaction where
--     toHash = StringHash . Base64.encode . SHA.hash . S.encode

