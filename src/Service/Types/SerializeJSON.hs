{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PackageImports           #-}
{-# LANGUAGE ScopedTypeVariables      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Service.Types.SerializeJSON where


import           Control.Monad
import qualified "cryptonite" Crypto.PubKey.ECC.ECDSA as ECDSA
import           Data.Aeson
import           Data.Aeson.Types                     (typeMismatch)
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Char8                as BS

import qualified Data.ByteString.Base64               as B
import           Data.ByteString.Conversion
import           Data.Text                            (Text, pack, unpack)
import qualified Data.Text.Encoding                   as T (decodeUtf8,
                                                            encodeUtf8)
import           Service.Types
import           Service.Types.PublicPrivateKeyPair


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


decodeFromText :: (MonadPlus m) => Text -> m ByteString
decodeFromText aStr = case B.decode . T.encodeUtf8 $ aStr of
    Right a -> return a
    Left _  -> mzero

intToBase64Text :: Integer -> Text
intToBase64Text i = encodeToText $ toByteString' i

base64TextToInt :: (MonadPlus m) => Text -> m Integer
base64TextToInt b = do
     bs <- decodeFromText b
     case fromByteString bs of
       Just i -> return i
       _      -> mzero


instance ToJSON Hash
instance FromJSON Hash

instance ToJSON ByteString where
  toJSON h = String $ pack $ BS.unpack h

instance FromJSON ByteString where
  parseJSON (String s) = return $ BS.pack $ unpack s
  parseJSON _          = error "Wrong object format"

instance ToJSON TransactionInfo where
  toJSON t = object [
      "tx"       .= _tx (t :: TransactionInfo)
    , "block"    .= _block t
    , "index"    .= _index t
    , "accepted" .= _accepted t
    ]

instance ToJSON MicroblockBD where
  toJSON mb = object [
      "keyBlock"            .= _keyBlock (mb :: MicroblockBD)
    , "signBD"              .= _signBD (mb :: MicroblockBD)
    , "publisher"           .= _publisher (mb :: MicroblockBD)
    , "transactionsHashes"  .= _transactionsHashes (mb :: MicroblockBD)
    ]

instance FromJSON MicroblockBD where
    parseJSON (Object aMsg) = do
        aKeyBlock           <- aMsg .: "keyBlock"
        aSignBd             <- aMsg .: "signBD"
        aPublisher          <- aMsg .: "publisher"
        aTransactionHashes  <- aMsg .: "transactionsHashes"
        return $ MicroblockBD aKeyBlock aSignBd aPublisher aTransactionHashes

    parseJSON _ = mzero

instance FromJSON TransactionInfo where
  parseJSON (Object o) = TransactionInfo
    <$> o .: "tx"
    <*> o .: "block"
    <*> o .: "index"
    <*> o .: "accepted"
  parseJSON inv        = typeMismatch "TransactionInfo" inv

instance ToJSON ECDSA.Signature where
  toJSON t = object [
    "sign_r" .= intToBase64Text  (ECDSA.sign_r t),
    "sign_s" .= intToBase64Text  (ECDSA.sign_s t) ]

instance FromJSON ECDSA.Signature where
  parseJSON (Object v) = do
    s_r <- base64TextToInt =<< v .: "sign_r"
    s_s <- base64TextToInt =<< v .: "sign_s"
    return $ ECDSA.Signature s_r s_s
  parseJSON inv        = typeMismatch "Signature" inv


instance ToJSON TransactionAPI where
   toJSON tx = object  [
             "tx"   .= _tx (tx :: TransactionAPI)
           , "hash" .= _txHash tx
           ]

instance FromJSON TransactionAPI where
   parseJSON (Object o) = TransactionAPI
           <$> o .: "tx"
           <*> o .: "hash"
   parseJSON inv        = typeMismatch "TransactionAPI" inv

instance ToJSON Transaction where
   toJSON tx = object $ [
           "owner"     .= _owner tx,
           "receiver"  .= _receiver tx,
           "amount"    .= _amount tx,
           "currency"  .= _currency tx,
           "uuid"      .= _uuid tx
           ]
       ++ case _timestamp (tx :: Transaction) of
            Nothing -> []
            Just t  -> [ "timestamp" .= t ]

       ++ case _signature (tx :: Transaction) of
            Nothing -> []
            Just s  -> [ "sign" .= s ]

instance FromJSON Transaction where
   parseJSON (Object o) = Transaction
              <$> o .:  "owner"
              <*> o .:  "receiver"
              <*> o .:  "amount"
              <*> o .:  "currency"
              <*> o .:? "timestamp"
              <*> o .:? "sign"
              <*> o .:  "uuid"
   parseJSON inv = typeMismatch "Transaction" inv

instance ToJSON MicroblockAPI where
    toJSON bl = object  [
            "prev_block"   .= _prevMicroblock (bl :: MicroblockAPI)
         ,  "next_block"   .= _nextMicroblock (bl :: MicroblockAPI)
         ,  "k_block"      .= _keyBlock (bl :: MicroblockAPI)
         -- ,  "team"         .= _teamKeys (bl :: MicroblockAPI)
         ,  "publisher"    .= _publisher (bl :: MicroblockAPI)
         ,  "sign"         .= _signAPI (bl :: MicroblockAPI)
         ,  "transactions" .= _transactionsAPI bl
       ]

instance FromJSON MicroblockAPI where
    parseJSON (Object o) = MicroblockAPI
               <$> o .: "prev_block"
               <*> o .: "next_block"
               <*> o .: "k_block"
               <*> o .: "sign"
               -- <*> o .: "team"
               <*> o .: "publisher"
               <*> o .: "transactions"
    parseJSON inv         = typeMismatch "MicroblockAPI" inv

instance ToJSON MicroblockInfoAPI where
    toJSON bl = object  [
            "prev_block"   .= _prevMicroblock (bl :: MicroblockInfoAPI)
         ,  "next_block"   .= _nextMicroblock (bl :: MicroblockInfoAPI)
         ,  "k_block"      .= _keyBlock (bl :: MicroblockInfoAPI)
         -- ,  "team"         .= _teamKeys (bl :: MicroblockInfoAPI)
         ,  "publisher"    .= _publisher (bl :: MicroblockInfoAPI)
         ,  "sign"         .= _signAPI (bl :: MicroblockInfoAPI)
         ,  "hash"         .= _hash (bl :: MicroblockInfoAPI)
       ]

instance FromJSON MicroblockInfoAPI where
    parseJSON (Object o) = MicroblockInfoAPI
               <$> o .: "prev_block"
               <*> o .: "next_block"
               <*> o .: "k_block"
               <*> o .: "sign"
               -- <*> o .: "team"
               <*> o .: "publisher"
               <*> o .: "hash"
    parseJSON inv         = typeMismatch "MicroblockInfo" inv


instance ToJSON Microblock where
 toJSON aBlock = object [
       "msg" .= object [
           "K_hash"    .= _keyBlock (aBlock :: Microblock),
           "wallets"   .= _teamKeys (aBlock :: Microblock),
           "Tx"        .= _transactions aBlock,
           "publisher" .= _publisher (aBlock :: Microblock)
         ],
       "sign" .= _sign (aBlock :: Microblock)
   ]


instance FromJSON Microblock where
 parseJSON (Object v) = do
     aMsg  <- v .: "msg"
     aSign <- v .: "sign"
     case aMsg of
       Object aBlock -> do
           aWallets   <- aBlock .: "wallets"
           aTx        <- aBlock .: "Tx"
           aPublisher <- aBlock .: "publisher"
           aKhash     <- aBlock .: "K_hash"
           return $ Microblock aKhash aSign aWallets aPublisher aTx
       _ -> mzero
 parseJSON _ = mzero


instance ToJSON MacroblockBD where
    toJSON bl = object  [
            "prev_hash"         .= _prevKBlock (bl :: MacroblockBD)
         ,  "next_hash"         .= _nextKBlock (bl :: MacroblockBD)
         ,  "prev_Khash"        .= _prevHKBlock (bl :: MacroblockBD)
         ,  "difficulty"        .= _difficulty (bl :: MacroblockBD)
         ,  "height"            .= _height (bl :: MacroblockBD)
         ,  "solver"            .= _solver (bl :: MacroblockBD)
         ,  "reward"            .= _reward (bl :: MacroblockBD)
         ,  "timeK"             .= _time (bl :: MacroblockBD)
         ,  "numberK"           .= _number (bl :: MacroblockBD)
         ,  "nonce"             .= _nonce (bl :: MacroblockBD)
         ,  "microblocks"       .= _mblocks (bl :: MacroblockBD)
         ,  "team_keys"         .= _teamKeys (bl :: MacroblockBD)
       ]

instance FromJSON MacroblockBD where
    parseJSON (Object o) = MacroblockBD
               <$> o .:? "prev_hash"
               <*> o .:? "next_hash"
               <*> o .:? "prev_Khash"
               <*> o .: "difficulty"
               <*> o .: "height"
               <*> o .: "solver"
               <*> o .: "reward"
               <*> o .: "timeK"
               <*> o .: "numberK"
               <*> o .: "nonce"
               <*> o .: "microblocks"
               <*> o .: "team_keys"
    parseJSON inv         = typeMismatch "MacroblockBD" inv


instance ToJSON MacroblockAPI where
    toJSON bl = object  [
            "prev_hash"         .= _prevKBlock (bl :: MacroblockAPI)
         ,  "next_hash"         .= _nextKBlock (bl :: MacroblockAPI)
         ,  "difficulty"        .= _difficulty (bl :: MacroblockAPI)
         ,  "height"            .= _height (bl :: MacroblockAPI)
         ,  "solver"            .= _solver (bl :: MacroblockAPI)
         ,  "reward"            .= _reward (bl :: MacroblockAPI)
--         ,  "microblocks_cnt"   .= length (_mblocksAPI bl)
         ,  "microblocks"       .= _mblocks (bl :: MacroblockAPI)
         ,  "team_keys"         .= _teamKeys (bl :: MacroblockAPI)
       ]

instance FromJSON MacroblockAPI where
    parseJSON (Object o) = MacroblockAPI
               <$> o .: "prev_hash"
               <*> o .: "next_hash"
               <*> o .: "difficulty"
               <*> o .: "height"
               <*> o .: "solver"
               <*> o .: "reward"
               <*> o .: "microblocks"
               <*> o .: "team_keys"
    parseJSON inv         = typeMismatch "MacroblockAPI" inv

instance ToJSON ChainInfo where
    toJSON info = object  [
          "emission"   .= _emission info
        , "difficulty" .= _curr_difficulty info
        , "last_block" .= _last_block info
        , "blocks_num" .= _blocks_num info
        , "txs_num"    .= _txs_num info
        , "nodes_num"  .= _nodes_num info
      ]

instance FromJSON ChainInfo where
    parseJSON (Object o) = ChainInfo
               <$> o .: "emission"
               <*> o .: "difficulty"
               <*> o .: "last_block"
               <*> o .: "blocks_num"
               <*> o .: "txs_num"
               <*> o .: "nodes_num"
    parseJSON inv        = typeMismatch "ChainInfo" inv


instance FromJSON KeyBlockInfo where
  parseJSON (Object v) = KeyBlockInfo
                         <$> (v .: "time")
                         <*> (v .: "prev_hash")
                         <*> (v .: "number")
                         <*> (v .: "nonce")
                         <*> (v .: "solver")
                         <*> (v .: "type")
  parseJSON inv        = typeMismatch "KeyBlockInfo" inv

instance FromJSON KeyBlockInfoPoW where
  parseJSON (Object v) = KeyBlockInfoPoW
                         <$> (v .: "time")
                         <*> (v .: "prev_hash")
                         <*> (v .: "number")
                         <*> (v .: "nonce")
                         <*> (v .: "solver")
                         <*> (v .: "type")
  parseJSON inv        = typeMismatch "KeyBlockInfo" inv
