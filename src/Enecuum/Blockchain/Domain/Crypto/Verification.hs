{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Enecuum.Blockchain.Domain.Crypto.Verification where

import           "cryptonite" Crypto.Hash (SHA3_256(..))
import           Crypto.PubKey.ECC.ECDSA (Signature, PublicKey,verify)
import           Data.Serialize (Serialize, encode)
import           Enecuum.Prelude

verifyEncodable :: Serialize msg => PublicKey -> Signature -> msg -> Bool
verifyEncodable publicKey signature msg = verify SHA3_256 publicKey signature (encode msg)
