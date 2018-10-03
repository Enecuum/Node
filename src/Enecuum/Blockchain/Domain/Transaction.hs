
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

type TransactionID = Int

data Transaction = Transaction
    { _prevHash    :: StringHash
    , _change      :: Int
    , _owner     :: Legacy.PublicKey
    , _receiver  :: Legacy.PublicKey
    -- , _amount    :: Legacy.Amount
    , _currency  :: Legacy.Currency
    , _timestamp :: Maybe Legacy.Time
    , _signature :: Maybe Legacy.Signature
    -- , _uuid      :: Int    
    }
  deriving ( Generic, Show, Eq, Ord, Read)  

instance S.Serialize Transaction

-- instance StringHashable Transaction where
--     toHash = StringHash . Base64.encode . SHA.hash . S.encode


type Balance = Int
type BalanceChange = Int


dummyTx = Transaction { 
  _prevHash  = toHash @Int 0
, _change    = 0
, _owner     = (1 :: Legacy.PublicKey)
, _receiver  = (2 :: Legacy.PublicKey)
, _currency  = Legacy.ENQ
, _timestamp = Nothing
, _signature = Nothing
}