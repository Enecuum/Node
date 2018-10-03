module Enecuum.Blockchain.Domain.Microblock where

import Enecuum.Prelude

import Data.HGraph.StringHashable (StringHash (..), StringHashable, toHash)

import qualified Crypto.Hash.SHA256         as SHA
import qualified Data.ByteString.Base64     as Base64
import qualified Data.Serialize             as S

import Enecuum.Blockchain.Domain.Transaction (Transaction)


data Microblock = Microblock
    { _keyBlock     :: StringHash
    , _transactions :: [Transaction]
    }
    deriving (Eq, Generic, Ord, Read, Show)


instance Serialize Microblock

instance StringHashable Microblock where
  toHash = StringHash . Base64.encode . SHA.hash . S.encode
