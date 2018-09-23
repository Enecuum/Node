
module Enecuum.Blockchain.Domain.Transaction where

import Enecuum.Prelude

import qualified Data.ByteString.Base64  as Base64
import qualified Data.Serialize          as S
import qualified Crypto.Hash.SHA256      as SHA

import           Data.HGraph.StringHashable (StringHash (..), StringHashable, toHash)

-- This data structure is for tests of graph incorporation only.
-- Please, replace it by actual blockchain data.

type TransactionID = Int

data Transaction = Transaction
    { _prevHash    :: StringHash
    , _change      :: Int
    }
  deriving (Generic)

instance S.Serialize Transaction

instance StringHashable Transaction where
    toHash = StringHash . Base64.encode . SHA.hash . S.encode


type Balance = Int
type BalanceChange = Int
