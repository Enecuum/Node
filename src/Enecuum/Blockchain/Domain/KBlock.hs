{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}

module Enecuum.Blockchain.Domain.KBlock where

import qualified Crypto.Hash.SHA256         as SHA
import qualified Data.ByteString            as B
import qualified Data.ByteString.Base64     as Base64
import qualified Data.ByteString.Internal   as BSI
import           Data.HGraph.StringHashable (StringHash (..), StringHashable, toHash, fromStringHash)
import           Data.Serialize.Put         (putWord32le, putWord8, runPut)
import           Enecuum.Prelude

data KBlock = KBlock
    { _time      :: Integer
    , _prevHash  :: StringHash
    , _number    :: Integer
    , _nonce     :: Integer
    , _solver    :: StringHash
    -- , _type      :: Int
    } deriving (Eq, Generic, Ord, Read, Show, ToJSON, FromJSON, Serialize)

instance StringHashable KBlock where
  toHash = StringHash . calculateKeyBlockHash

genesisHash :: StringHash
genesisHash = toHash genesisKBlock

genesisIndicationHash :: IsString a => a
genesisIndicationHash = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="

genesisSolver :: IsString a => a
genesisSolver = "EMde81cgGToGrGWSNCqm6Y498qBpjEzRczBbvC5MV2Q="

kBlockType :: Int
kBlockType = 0

genesisKBlock :: KBlock
genesisKBlock = KBlock
    { _time      = 0
    , _prevHash  = StringHash genesisIndicationHash
    , _number    = 0
    , _nonce     = 0
    , _solver    = StringHash genesisSolver
    }

calculateKeyBlockHash :: KBlock -> BSI.ByteString
calculateKeyBlockHash KBlock {..} = Base64.encode . SHA.hash . B.concat $ bstr
  where
    bstr = map runPut
            [ putWord8 (toEnum kBlockType)
            , putWord32le (fromInteger _number)
            , putWord32le (fromInteger _time)
            , putWord32le (fromInteger _nonce)
            ]
            ++
            [ fromRight "" $ Base64.decode $ fromStringHash _prevHash
            , fromRight "" $ Base64.decode $ fromStringHash _solver
            ]

