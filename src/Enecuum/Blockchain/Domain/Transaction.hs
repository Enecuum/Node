
module Enecuum.Blockchain.Domain.Transaction where

import Enecuum.Prelude

import qualified Data.ByteString.Base64  as Base64
import qualified Data.Serialize          as S
import qualified Crypto.Hash.SHA256      as SHA

import           Data.HGraph.StringHashable (StringHash (..), StringHashable, toHash)
import qualified Enecuum.Legacy.Refact.Crypto.PublicPrivateKeyPair as Legacy 
import qualified Enecuum.Legacy.Service.Types as Legacy 


-- This data structure is for tests of graph incorporation only.
-- Please, replace it by actual blockchain data.

-- type TransactionID = Int

data Transaction = Transaction
    { _owner     :: Int
    , _receiver  :: Int
    , _amount    :: Int
    }
  deriving ( Generic, Show, Eq, Ord, Read)  

instance S.Serialize Transaction

-- instance StringHashable Transaction where
--     toHash = StringHash . Base64.encode . SHA.hash . S.encode


type Balance = Int
type BalanceChange = Int


dummyTx = Transaction
    { _amount    = 0
    , _owner     = 1
    , _receiver  = 2
    }