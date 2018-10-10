{-# LANGUAGE DeriveAnyClass #-}

module Enecuum.Blockchain.Domain.Transaction where

import Enecuum.Prelude

import qualified Data.ByteString.Base64  as Base64
import qualified Data.Serialize          as S
import qualified Crypto.Hash.SHA256      as SHA

import           Data.HGraph.StringHashable (StringHash (..), StringHashable, toHash)
import Enecuum.Blockchain.Domain.Crypto 


-- This data structure is for tests of graph incorporation only.
-- Please, replace it by actual blockchain data.

-- type TransactionID = Int

data Transaction = Transaction
    { _owner     :: PublicKey
    , _receiver  :: PublicKey
    , _amount    :: Integer
    }
  deriving ( Generic, Show, Eq, Ord, Read, ToJSON, FromJSON)

instance S.Serialize Transaction

-- instance StringHashable Transaction where
--     toHash = StringHash . Base64.encode . SHA.hash . S.encode


type Balance = Int
type BalanceChange = Int

dummyTx = Transaction 
 {
   _amount = 0, 
   _owner = read "QYy3AT4a3Z88MpEoGDixRgxtWW8v3RfSbJLFQEyFZwMe" :: PublicKey, 
   _receiver = read "pYeXNM7cn2B6A68rH9PYLCCgrXWiVbucfNW1XMW3Q4G" :: PublicKey 
  }
