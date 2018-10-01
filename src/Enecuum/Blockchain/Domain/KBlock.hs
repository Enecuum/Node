{-# LANGUAGE DeriveAnyClass #-}

module Enecuum.Blockchain.Domain.KBlock where

import Enecuum.Prelude

import qualified Data.ByteString.Base64  as Base64
import qualified Data.Serialize          as S
import qualified Crypto.Hash.SHA256      as SHA

import           Data.HGraph.StringHashable (StringHash (..), StringHashable, toHash)

-- This data structure is for tests of graph incorporation only.
-- Please, replace it by actual blockchain data.

type KBlockID = Int

data KBlock = Block
    {
      _id        :: KBlockID
    }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

instance S.Serialize KBlock

instance StringHashable KBlock where
    toHash = StringHash . Base64.encode . SHA.hash . S.encode
