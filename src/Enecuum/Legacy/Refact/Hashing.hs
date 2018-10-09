{-# LANGUAGE RecordWildCards #-}

module Enecuum.Legacy.Refact.Hashing where

import qualified Crypto.Hash.SHA256                                as SHA
import qualified Data.ByteString                                   as B
import qualified Data.ByteString.Base64                            as Base64
import qualified Data.ByteString.Internal                          as BSI
import qualified Data.Serialize                                    as S (encode)
import           Data.Serialize.Put (putWord8, putWord32le, runPut)
import           Enecuum.Legacy.Service.Types ( KeyBlockInfoPoW (..) )
import           Prelude
import           Data.Either

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
            ++ [fromRight "" $ Base64.decode _prev_hash, fromRight "" $ Base64.decode _solver]
