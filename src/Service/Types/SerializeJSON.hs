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
import Data.ByteString (ByteString)
import Data.Text (Text, pack, unpack)
import qualified Data.ByteString.Base16 as B
import qualified Data.Text.Encoding as T (encodeUtf8, decodeUtf8)

instance FromJSON Trans
instance ToJSON   Trans

instance FromJSON MsgTo
instance ToJSON MsgTo

instance FromJSON Currency
instance ToJSON Currency

instance FromJSON PublicKey where
  parseJSON (String s) = return $ read $ unpack s
  parseJSON _          = error "PublicKey JSON parse error"

instance ToJSON PublicKey where
  toJSON key = String $ pack $ show key

instance FromJSON PrivateKey
instance ToJSON PrivateKey


encodeToText :: ByteString -> Text
encodeToText = T.decodeUtf8 . B.encode

decodeFromText :: (Monad m) => Text -> m ByteString
decodeFromText = return . fst . B.decode . T.encodeUtf8


instance ToJSON Hash where
  toJSON (Hash h) = object [
                  "hash" .= encodeToText h
                ]

instance FromJSON Hash where
  parseJSON (Object v) = Hash <$> ((v .: "hash") >>= decodeFromText)
  parseJSON _          = error "Hash JSON parse error"


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
  parseJSON _          = error "TransactionInfo JSON parse error"



instance ToJSON Microblock where
  toJSON aBlock = undefined
{-
      object [
        "msg" := object [
            "K_hash"  := _keyBlock aBlock,
            "wallets" := _teamKeys aBlock,
            "Tx"      := _transactions aBlock,
          ]
    ]
-}
{-

data Microblock = Microblock{
    _keyBlock :: ByteString, -- hash of key-block
    _signer :: PublicKey,
    _sign :: Signature,  -- signature for {K_hash, [Tx],}
    _teamKeys :: [PublicKey], -- for reward
    _transactions :: [Transaction]}
  deriving (Eq, Generic, Ord, Read)

{
    "msg":{
        "K_hash":"SoMeBaSe64StRinG==",
        "wallets":[
            "SoMeBaSe64StRinG==",
            "SoMeBaSe64StRinG==",
            ...
        ],
        "Tx":[{
                "from":"SoMeBaSe64StRinG==",
                "to":"SoMeBaSe64StRinG==",
                "amount":<uint>,
                "uuid":"SoMeBaSe64StRinG=="
            },
            ...
        ],
        "i":<uint>
    },
    "sign":"SoMeBaSe64StRinG=="
}
-}
{-
object [
                   "curr"  .= encodeToText (hashCurrentMicroblock block)
                 , "prev"  .= encodeToText (hashPreviousMicroblock block)
                 , "txs"   .= trans block
                 ]
-}
instance FromJSON MicroblockV1 where
  parseJSON (Object v) = undefined
      {-MicroblockV1
                           <$> ((v .: "curr") >>= decodeFromText)
                           <*> ((v .: "prev") >>= decodeFromText)
                           <*> v .: "txs"
-}

instance FromJSON Microblock where
  parseJSON (Object v) = undefined
      {-MicroblockV1
                           <$> ((v .: "curr") >>= decodeFromText)
                           <*> ((v .: "prev") >>= decodeFromText)
                           <*> v .: "txs"
-}

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
    toJSON tx = object  [
            "owner"     .= _owner tx,
            "receiver"  .= _receiver tx,
            "amount"    .= _amount tx,
            "currency"  .= _currency tx,
            "timestamp" .= _time tx,
            "sign"       .= _signature tx
          ]

instance FromJSON Transaction where
    parseJSON (Object o) = Transaction
               <$> o .: "owner"
               <*> o .: "receiver" 
               <*> o .: "amount"
               <*> o .: "currency"
               <*> o .: "timestamp"
               <*> o .: "sign"
    parseJSON inv         = typeMismatch "Transaction" inv
