{-# LANGUAGE DeriveAnyClass #-}

module Enecuum.Blockchain.Domain.KBlock where

import           Enecuum.Prelude

import qualified Crypto.Hash.SHA256         as SHA
import qualified Data.ByteString.Base64     as Base64
import qualified Data.Serialize             as S

import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BS
import           Data.HGraph.StringHashable (StringHash (..), StringHashable,
                                             toHash)
import           Prelude                    (String)
-- import              Enecuum.Legacy.Service.Types
--     ,   KeyBlockInfo(..)
--     )


-- This data structure is for tests of graph incorporation only.
-- Please, replace it by actual blockchain data.
-- type KBlock = KeyBlockInfoPoW

-- type KBlockID = Int
-- data KBlock = Block
--     {
--       _id        :: KBlockID
--     , _prevKBlockHash  :: StringHash
--     }
--   deriving (Generic, Show, Eq, ToJSON, FromJSON)

-- instance S.Serialize KBlock


instance ToJSON StringHash where
  toJSON h = undefined
  -- toJSON h = StringHash $ toJSON h

instance FromJSON StringHash where
  parseJSON b =  undefined
  -- parseJSON b = parseJSON (StringHash b)


data KBlock = KBlock {
    _time       :: Integer
  , _prev_hash  :: StringHash
  , _number     :: Integer
  , _nonce      :: Integer
  , _solver     :: StringHash
  , _typeKBlock :: Int
  } deriving (Eq, Generic, Ord, Read, Show, ToJSON, FromJSON)
instance Serialize KBlock
