{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Enecuum.Legacy.Refact.Crypto.Verification where

import           "cryptonite" Crypto.Hash
import           Crypto.PubKey.ECC.ECDSA
import           Data.Serialize
import           Prelude

verifyEncodeble :: Serialize msg => PublicKey -> Signature -> msg -> Bool
verifyEncodeble publicKey signature msg = verify SHA3_256
    publicKey signature (encode msg)
