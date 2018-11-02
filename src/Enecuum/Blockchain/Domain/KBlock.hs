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

type Time'    = Integer
type Number   = Integer
type Nonce    = Integer
type Solver   = StringHash
type PrevHash = StringHash

data KBlock = KBlock
    { _time      :: Time'
    , _prevHash  :: PrevHash
    , _number    :: Number
    , _nonce     :: Nonce
    , _solver    :: Solver
    -- , _type      :: Int
    } deriving (Eq, Generic, Ord, Read, Show, ToJSON, FromJSON, Serialize)

instance StringHashable KBlock where
  toHash = StringHash . calculateKeyBlockHash

genesisHash :: StringHash
genesisHash = toHash genesisKBlock

genesisIndicationHashStr :: IsString a => a
genesisIndicationHashStr = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="

genesisIndicationHash :: StringHash
genesisIndicationHash = StringHash genesisIndicationHashStr

genesisSolverStr :: IsString a => a
genesisSolverStr = "EMde81cgGToGrGWSNCqm6Y498qBpjEzRczBbvC5MV2Q="

genesisSolverHash :: StringHash
genesisSolverHash = StringHash genesisSolverStr

kBlockType :: Int
kBlockType = 0

genesisKBlock :: KBlock
genesisKBlock = KBlock
    { _time      = 0
    , _prevHash  = genesisIndicationHash
    , _number    = 0
    , _nonce     = 0
    , _solver    = genesisSolverHash
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

