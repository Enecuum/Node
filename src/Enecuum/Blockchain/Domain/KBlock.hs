{-# LANGUAGE DeriveAnyClass #-}

module Enecuum.Blockchain.Domain.KBlock where

import qualified Crypto.Hash.SHA256         as SHA
import qualified Data.ByteString.Base64     as Base64
import           Data.HGraph.StringHashable (StringHash (..), StringHashable, toHash)
import qualified Data.Serialize             as S
import           Enecuum.Prelude

data KBlock = KBlock
  { _prevHash :: StringHash
  , _number   :: Integer
  , _nonce    :: Integer
  , _solver   :: StringHash
  } deriving (Eq, Generic, Ord, Read, Show, ToJSON, FromJSON)


instance Serialize KBlock

instance StringHashable KBlock where
  toHash = StringHash . Base64.encode . SHA.hash . S.encode
