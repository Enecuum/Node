{-# LANGUAGE
        OverloadedStrings
    ,   PackageImports
  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Service.Types.SerializeJSON where

import              Data.Aeson
import              Data.Aeson.Types (typeMismatch)
import qualified "cryptonite"   Crypto.PubKey.ECC.ECDSA     as ECDSA
import Service.Types.PublicPrivateKeyPair
import Service.Types

instance FromJSON CryptoCurrency
instance ToJSON CryptoCurrency

instance FromJSON PublicKey
instance ToJSON PublicKey

instance FromJSON PrivateKey
instance ToJSON PrivateKey

instance ToJSON ECDSA.Signature where
  toJSON t = object [
    "sign_r" .= ECDSA.sign_r t,
    "sign_s" .= ECDSA.sign_s t ]

instance FromJSON ECDSA.Signature where
 parseJSON (Object v) =
    ECDSA.Signature <$> v .: "sign_r"
                    <*> v .: "sign_s"
 parseJSON inv        = typeMismatch "Signature" inv

instance ToJSON Transaction where
    toJSON trans = object $ txToJSON trans
        where
        txToJSON (WithTime aTime tx)
            = ("time" .= aTime) : txToJSON tx
        txToJSON (WithSignature tx sign)                   = txToJSON tx ++ [ "signature" .= sign ]
        txToJSON (RegisterPublicKey key aBalance)
            = [ "public_key" .= key, "start_balance" .= aBalance]
        txToJSON (SendAmountFromKeyToKey own aRec anAmount) = [
            "owner_key"     .= own,
            "receiver_key"  .= aRec,
            "amount"        .= anAmount
          ]

instance FromJSON Transaction where
    parseJSON (Object o) = do
               aTime    <- o .:? "time"
               sign     <- o .:? "signature"
               p_key    <- o .:? "public_key"
               aBalance <- o .:? "start_balance"
               o_key    <- o .:? "owner_key"
               r_key    <- o .:? "receiver_key"
               anAmount <- o .:? "amount"
               return $ appTime aTime
                      $ appSign sign
                      $ pack p_key aBalance o_key r_key anAmount
                 where
                   pack (Just p) (Just b) _ _ _ = RegisterPublicKey p b
                   pack _ _ (Just aO) (Just r) (Just a) = SendAmountFromKeyToKey aO r a
                   pack _ _ _ _ _ = error "Service.Types.SerializeJSON.parseJSON.pack"

                   appTime (Just t) trans = WithTime t trans
                   appTime  _ trans       = trans
                   appSign (Just s) trans = WithSignature trans s
                   appSign  _ trans       = trans
    parseJSON inv         = typeMismatch "Transaction" inv
