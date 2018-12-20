{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Samples.Blockchain.Domain.Microblock where

import qualified Crypto.Hash.SHA256                    as SHA
import qualified Data.ByteString.Base64                as Base64
import           Data.HGraph.StringHashable            (StringHash (..), StringHashable, toHash)
import qualified Data.Serialize                        as S
import           Enecuum.Samples.Blockchain.Domain.Transaction (Transaction)
import           Enecuum.Core.Crypto.Crypto
import           Enecuum.Prelude


data Microblock = Microblock
    { _keyBlock     :: StringHash
    , _transactions :: [Transaction]
    , _publisher    :: PublicKey
    , _signature    :: Signature
    }
    deriving (Eq, Generic, Ord, Read, Show, ToJSON, FromJSON, Serialize)

instance StringHashable Microblock where
  toHash = StringHash . Base64.encode . SHA.hash . S.encode