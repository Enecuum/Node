{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PackageImports #-}

module Enecuum.Legacy.Refact.Crypto.Signing where

import           "cryptonite" Crypto.Hash.Algorithms             ( SHA3_256 (..) )
import qualified "cryptonite" Crypto.PubKey.ECC.ECDSA            as ECDSA
import           "cryptonite" Crypto.Random                      ( MonadRandom )
import           Data.Serialize                                  ( Serialize, encode )

import qualified Enecuum.Legacy.Service.Types.PublicPrivateKeyPair as Enq

sign :: (Serialize msg, MonadRandom m) => Enq.PrivateKey -> msg -> m ECDSA.Signature
sign priv msg = ECDSA.sign (Enq.getPrivateKey priv) SHA3_256 (encode msg)
