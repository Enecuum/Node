{-# LANGUAGE
        OverloadedStrings
    ,   PackageImports
    ,   DisambiguateRecordFields
    ,   DuplicateRecordFields
    ,   ScopedTypeVariables
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
import Data.Maybe (fromJust)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.ByteString.Base58
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


decodeFromText :: (MonadPlus m) => Text -> m ByteString
decodeFromText aStr = case B.decode . T.encodeUtf8 $ aStr of
    Right a -> return a
    Left _  -> mzero


instance ToJSON Hash
instance FromJSON Hash




instance ToJSON ByteString where
  toJSON h = String $ decodeUtf8 $ encodeBase58 bitcoinAlphabet h

instance FromJSON ByteString where
  parseJSON (String s) = return $ fromJust $ decodeBase58 bitcoinAlphabet $ encodeUtf8 s
  parseJSON _          = error "Wrong object format"

instance ToJSON TransactionInfo
instance FromJSON TransactionInfo






instance FromJSON MicroblockV1 where
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
           "sign"      .= _signature tx,
           "uuid"      .= _uuid tx
           ]

instance FromJSON Transaction where
   parseJSON (Object o) = Transaction
              <$> o .: "owner"
              <*> o .: "receiver"
              <*> o .: "amount"
              <*> o .: "currency"
              <*> o .: "timestamp"
              <*> o .: "sign"
              <*> o .: "uuid"


instance ToJSON MicroblockAPI where
    toJSON bl = object  [
            "k_block"      .= _keyBlockAPI bl
         ,  "index"        .= _numOfBlockAPI bl
         ,  "publishers"   .= _teamKeysAPI bl
         ,  "reward"       .= (1 :: Integer)  -- fix or remove
         ,  "sign"         .= _signAPI bl
         ,  "txs_cnt"      .= length (_transactionsAPI bl)
         ,  "transactions" .= _transactionsAPI bl
       ]

instance FromJSON MicroblockAPI where
    parseJSON (Object o) = MicroblockAPI
               <$> o .: "k_block"
               <*> o .: "sign"
               <*> o .: "publishers"
               <*> o .: "transactions"
               <*> o .: "index"
    parseJSON inv         = typeMismatch "Microblock" inv


instance ToJSON Microblock where
 toJSON aBlock = object [
       "msg" .= object [
           "K_hash"  .= encodeToText (_keyBlock aBlock),
           "wallets" .= _teamKeys aBlock,
           "Tx"      .= _transactions aBlock
--           "uuid"    .= _numOfBlock aBlock
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
           -- aUuid    <- aBlock .: "uuid"
           aKhash   <- decodeFromText =<< aBlock .: "K_hash"
           return $ Microblock aKhash aSign aWallets aTx 0
       a -> mzero
parseJSON _ = mzero


instance ToJSON Macroblock where
    toJSON bl = object  [
            "prev_hash"         .= _prevBlock bl
         ,  "difficulty"        .= _difficulty bl
         ,  "height"            .= _height bl
         ,  "solver"            .= _solver bl
         ,  "reward"            .= _reward bl
         ,  "txs_cnt"           .= _txs_cnt bl
         ,  "microblocks_cnt"   .= length (_mblocks bl)
         ,  "microblocks"       .= _mblocks bl
       ]

instance FromJSON Macroblock where
    parseJSON (Object o) = Macroblock
               <$> o .: "prev_hash"
               <*> o .: "difficulty"
               <*> o .: "height"
               <*> o .: "solver"
               <*> o .: "reward"
               <*> o .: "txs_cnt"
               <*> o .: "microblocks"
    parseJSON inv         = typeMismatch "Macroblock" inv


instance ToJSON ChainInfo where
    toJSON info = object  [
          "emission"   .= _emission info
        , "difficulty" .= _curr_difficulty info
        , "blocks_num" .= _blocks_num info
        , "txs_num"    .= _txs_num info
        , "nodes_num"  .= _nodes_num info
      ]

instance FromJSON ChainInfo where
    parseJSON (Object o) = ChainInfo
               <$> o .: "emission"
               <*> o .: "difficulty"
               <*> o .: "blocks_num"
               <*> o .: "txs_num"
               <*> o .: "nodes_num"
    parseJSON inv        = typeMismatch "ChainInfo" inv
