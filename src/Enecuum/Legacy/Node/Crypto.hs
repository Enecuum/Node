{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Enecuum.Legacy.Node.Crypto where

import           Enecuum.Legacy.Node.Data.Key hiding (PublicKey (..))

import           Crypto.Cipher.Types
import           Crypto.Error
import           "cryptonite" Crypto.Hash
import           Crypto.PubKey.ECC.DH
import           Crypto.PubKey.ECC.ECDSA
import           Crypto.Random.Types

import           Crypto.Cipher.AES            (AES256)
import           Data.ByteArray               (unpack)

import           Data.ByteString              (ByteString, pack)
import           Data.Serialize
import           Prelude

verifyEncodeble :: Serialize msg => PublicKey -> Signature -> msg -> Bool
verifyEncodeble aPublicKey aSignature aMsg = verify SHA3_256
    aPublicKey aSignature (encode aMsg)
