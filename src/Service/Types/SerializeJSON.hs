{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Service.Types.SerializeJSON where

import              Data.Aeson
import              Data.Aeson.Types (typeMismatch)
import qualified "cryptonite"   Crypto.PubKey.ECC.ECDSA     as ECDSA
import Service.Types.PublicPrivateKeyPair
import Service.Types

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
        txToJSON (WithTime time tx)                        = [ "time" .= time ] ++ txToJSON tx
        txToJSON (WithSignature tx sign)                   = txToJSON tx ++ [ "signature" .= sign ]
        txToJSON (RegisterPublicKey key balance)           = [ "public_key" .= key, "start_balance" .= balance]
        txToJSON (SendAmountFromKeyToKey own rec amount)   = [ "owner_key" .= own,
                                                               "receiver_key" .= rec,
                                                               "amount" .= amount]

instance FromJSON Transaction where
    parseJSON (Object o) = do
               time    <- o .:? "time"
               sign    <- o .:? "signature"
               p_key   <- o .:? "public_key"
               balance <- o .:? "start_balance"
               o_key   <- o .:? "owner_key"
               r_key   <- o .:? "receiver_key"
               amount  <- o .:? "amount"
               return $ appTime time
                      $ appSign sign
                      $ pack p_key balance o_key r_key amount
                 where
                   pack (Just p) (Just b) _ _ _ = RegisterPublicKey p b
                   pack _ _ (Just o) (Just r) (Just a) = SendAmountFromKeyToKey o r a

                   appTime (Just t) trans = WithTime t trans
                   appTime  _ trans       = trans
                   appSign (Just s) trans = WithSignature trans s
                   appSign  _ trans       = trans
    parseJSON inv         = typeMismatch "Transaction" inv

