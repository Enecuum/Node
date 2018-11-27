{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}

module Enecuum.Blockchain.Domain.KBlock where

import qualified Crypto.Hash.SHA256         as SHA
import qualified Data.ByteString            as B
import qualified Data.ByteString.Base64     as Base64
import qualified Data.ByteString.Internal   as BSI
import           Data.HGraph.StringHashable (StringHash (..), StringHashable, fromStringHash, toHash)
import           Data.Serialize.Put         (putWord32le, putWord8, runPut)
import           Enecuum.Prelude

-- TODO: Base64 encoded newtype wrapper

-- TODO: this should be exact time type.
type BlockTime   = Word32
type BlockNumber = Word32
type Nonce       = Word32
type Solver      = StringHash
type PrevHash    = StringHash
type NonceRange  = (Nonce, Nonce)
type Difficulty  = Word32

data KBlock = KBlock
    { _time     :: BlockTime
    , _prevHash :: PrevHash
    , _number   :: BlockNumber
    , _nonce    :: Nonce
    , _solver   :: Solver
    -- , _type      :: Int
    } deriving (Eq, Generic, Ord, Read, Show, ToJSON, FromJSON, Serialize)

instance StringHashable KBlock where
  toHash = StringHash . calculateKeyBlockHash

data KBlockValidity
    = NextKBlock      -- ^ KBlock is good, and it's next to the current top
    | FutureKBlock    -- ^ KBlock is good, and it's from the future (number > curNumber + 1)
    | PreviousKBlock  -- ^ KBlock is duplicated
    | ForkedKBlock    -- ^ KBlock number <= curNumber, but we don't have this KBlock in the graph
    | InvalidKBlock   -- ^ KBlock is bad


genesisHash :: StringHash
genesisHash = toHash genesisKBlock

-- N.B. Hash in base64 encoding
genesisIndicationHashStr :: IsString a => a
genesisIndicationHashStr = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="

genesisIndicationHash :: StringHash
genesisIndicationHash = StringHash genesisIndicationHashStr

-- N.B. Hash in base64 encoding
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
            , putWord32le _number
            , putWord32le _time
            , putWord32le _nonce
            ]
            ++
            [ fromRight "" $ Base64.decode $ fromStringHash _prevHash
            , fromRight "" $ Base64.decode $ fromStringHash _solver
            ]
