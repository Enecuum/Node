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
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B
import Data.Text (Text)
import qualified Data.Text.Encoding as T (encodeUtf8, decodeUtf8)
import Data.Hex


instance FromJSON Trans
instance ToJSON   Trans

instance FromJSON MsgTo
instance ToJSON MsgTo

instance FromJSON CryptoCurrency
instance ToJSON CryptoCurrency

instance FromJSON PublicKey
instance ToJSON PublicKey

instance FromJSON PrivateKey
instance ToJSON PrivateKey


encodeToText :: ByteString -> Text
encodeToText = T.decodeUtf8 . B.encode


decodeFromText :: (MonadPlus m) => Text -> m ByteString
decodeFromText aStr = case B.decode . T.encodeUtf8 $ aStr of
    Right a -> return a
    Left _  -> mzero


instance ToJSON Hash where
  toJSON (Hash h) = object ["hash" .= encodeToText  h]

instance FromJSON Hash where
  parseJSON (Object v) = Hash <$> ((v .: "hash") >>= decodeFromText)


instance ToJSON TransactionInfo where
  toJSON info = object [
                  "tx"    .= tx info
                , "block" .= encodeToText (block info)
                , "index" .= index info
                ]

instance FromJSON TransactionInfo where
  parseJSON (Object v) = TransactionInfo
                           <$> v .: "tx"
                           <*> ((v .: "block") >>= decodeFromText)
                           <*> v .: "index"




instance ToJSON Microblock where
  toJSON aBlock = object [
        "msg" .= object [
            "K_hash"  .= encodeToText (_keyBlock aBlock),
            "wallets" .= _teamKeys aBlock,
            "Tx"      .= _transactions aBlock,
            "uuid"    .= _numOfBlock aBlock
          ],
        "sign" .= _sign aBlock
    ]


instance FromJSON Microblock where
  parseJSON (Object v) = do
      aMsg  <- v .: "msg"
      aSign <- v .: "sign"
      case aMsg of
        Object aBlock -> do
            aWallets <- aBlock .: "wallets"
            aTx      <- aBlock .: "Tx"
            aUuid    <- aBlock .: "i"
            aKhash   <- decodeFromText =<< aBlock .: "K_hash"
            return $ Microblock aKhash aSign aWallets aTx aUuid
        a -> mzero
  parseJSON _ = mzero

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
