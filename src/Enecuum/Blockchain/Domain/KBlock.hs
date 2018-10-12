{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}

module Enecuum.Blockchain.Domain.KBlock where

import qualified Crypto.Hash.SHA256         as SHA
import qualified Data.ByteString            as B
import qualified Data.ByteString.Base64     as Base64
import qualified Data.ByteString.Internal   as BSI
import           Data.HGraph.StringHashable (StringHash (..), StringHashable, toHash)
import qualified Data.Serialize             as S
import           Data.Serialize.Put         (putWord32le, putWord8, runPut)
import           Enecuum.Prelude

data KBlock = KBlock
  { _prevHash :: StringHash
  , _number   :: Integer
  , _nonce    :: Integer
  , _solver   :: StringHash
  } deriving (Eq, Generic, Ord, Read, Show, ToJSON, FromJSON, Serialize)


instance StringHashable KBlock where
  toHash = StringHash . Base64.encode . SHA.hash . S.encode


-- from PoW
data KeyBlockInfoPoW = KeyBlockInfoPoW {
    _time      :: Integer
  , _prevHash  :: ByteString
  , _number    :: Integer
  , _nonce     :: Integer
  , _solver    :: B.ByteString
  , _type      :: Int
  } deriving (Eq, Generic, Ord, Read, Show, Serialize)

calculateKeyBlockHash :: KeyBlockInfoPoW -> BSI.ByteString
calculateKeyBlockHash KeyBlockInfoPoW {..} = Base64.encode . SHA.hash $ bstr
  where
    bstr =
        B.concat
            $  map
                   runPut
                   [ putWord8 (toEnum _type)
                   , putWord32le (fromInteger _number)
                   , putWord32le (fromInteger _time)
                   , putWord32le (fromInteger _nonce)
                   ]
            ++ [fromRight "" $ Base64.decode _prevHash, fromRight "" $ Base64.decode _solver]

