{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PackageImports           #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}

module Service.Sync.SyncJson where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8            as BS
import qualified Data.Text                        as T
import           Node.Data.Key
import           Service.Transaction.SproutCommon (From, MicroBlockContent (..),
                                                   To)
import           Service.Types

deriving instance Show MicroBlockContent

type LastNumber = Int
type Count      = Int
type SyncStatusMessage  = String
type ErrorStringCode    = String



data SyncEvent where
    RestartSync ::                          SyncEvent
    SyncMsg     :: NodeId -> SyncMessage -> SyncEvent

data Chunk = Chunk KeyBlockInfoPoW [Microblock] deriving (Show)

data SyncMessage where
    RequestTail           ::                                             SyncMessage
    ResponseTail          :: Number                                   -> SyncMessage
    RequestChain          ::                                             SyncMessage
    ResponseChain         :: [Chunk]                                  -> SyncMessage
{-
    PeekHashKblokRequest  :: From             -> To                   -> SyncMessage
    PeekHashKblokResponse :: [(Number, HashOfKeyBlock)]               -> SyncMessage
    PeekKeyBlokRequest    :: From             -> To                   -> SyncMessage
    PeekKeyBlokResponse   :: [(Number, HashOfKeyBlock, MacroblockBD)] -> SyncMessage
    MicroblockRequest     :: HashOfMicroblock                         -> SyncMessage
    MicroblockResponse    :: MicroBlockContent                        -> SyncMessage
-}
    StatusSyncMessage     :: SyncStatusMessage -> ErrorStringCode     -> SyncMessage
  deriving (Show)


{-

{
    "sync": "chunk",
    "chunk": [
        {
            "block": {
                "time": 1533291189,
                "nonce": 218605,
                "number": 1,
                "type": 0,
                "prev_hash": "B1Vh7/LNOtWGd2+pBPAEAoLF9qJh9qj9agpSTRTNLSw=",
                "solver": ""
            },
            "microblocks": []
        }
    ]
}

-}
{-
instance FromJSON KeyBlockInfo where
  parseJSON (Object v) = KeyBlockInfoPoW
                         <$> (v .: "time")
                         <*> (v .: "prev_hash")
                         <*> (v .: "number")
                         <*> (v .: "nonce")
                         <*> (v .: "solver")
                         <*> (v .: "type")
  parseJSON inv        = typeMismatch "KeyBlockInfo" inv

-}
instance ToJSON SyncMessage where
    toJSON RequestTail = object [
        "sync"      .= ("tail"   :: String),
        "tail"      .= (0 :: Int)
      ]

    toJSON (ResponseTail aNumber) = object [
        "sync"        .= ("peak"   :: String),
        "tail"        .= aNumber
      ]

    toJSON RequestChain = object [
        "sync"        .= ("sync"   :: String),
        "tail"        .= (0 :: Int)
      ]
{-
    toJSON (PeekKeyBlokRequest aFrom aTo) = object [
        "sync"      .= ("peek_key_blok_request"   :: String),
        "from"      .= aFrom,
        "to"        .= aTo
      ]

    toJSON (PeekKeyBlokResponse aMacroblocksBD) = object [
        "sync"      .= ("peek_key_blok_response"   :: String),
        "kblocks"   .= aMacroblocksBD
      ]

    toJSON (MicroblockRequest aHashOfMicroblocks) = object [
        "sync"      .= ("block_mickro_blok_request"   :: String),
        "block_hash"   .= aHashOfMicroblocks
      ]
    toJSON (MicroblockResponse aMickroBlokContents) = object [
        "sync"      .= ("block_mickro_blok_response"   :: String),
        "block_hash"   .= aMickroBlokContents
      ]


    toJSON (PeekHashKblokRequest aCount aHashOfKeyBlock) = object [
        "sync"          .= ("peek_hash_key_blok_request"   :: String),
        "kblock_count"  .= aCount,
        "block_hash"    .= aHashOfKeyBlock
      ]

    toJSON (PeekHashKblokResponse aHashOfKeyBlock) = object [
        "sync"     .= ("peek_hash_key_blok_response"   :: String),
        "hashes"   .= aHashOfKeyBlock
      ]
-}
    toJSON (StatusSyncMessage msg errorCode) = object [
        "sync"      .= ("error"   :: String),
        "msg"       .= msg,
        "errorCode" .= errorCode
      ]

instance FromJSON Chunk where
    parseJSON (Object aMessage) = do
        aBlock       <- aMessage .: "block"
        aMicroBlocks <- aMessage .: "microblocks"
        return $ Chunk aBlock aMicroBlocks

instance FromJSON SyncMessage where
    parseJSON (Object aMessage) = do
        sync  :: T.Text <- aMessage .:"sync"
        case (T.unpack sync) of
            "tail" -> return RequestTail
            "peek" -> ResponseTail <$> aMessage .: "tail"
            "sync" -> return RequestChain
            "chunk"-> ResponseChain <$> aMessage .: "chunk"

{-
            "peek_key_blok_request"-> do
                from <- aMessage .: "from"
                to   <- aMessage .: "to"
                return $ PeekKeyBlokRequest from to

            "peek_key_blok_response"->
                PeekKeyBlokResponse <$> aMessage .: "kblocks"

            "block_mickro_blok_request"->
                MicroblockRequest <$> aMessage .: "block_hash"

            "block_mickro_blok_response"->
                MicroblockResponse <$> aMessage .: "block_hash"

            "peek_hash_key_blok_request"-> do
                aCount <- aMessage .: "kblock_count"
                lastHash   <- aMessage .: "block_hash"
                return $ PeekHashKblokRequest aCount lastHash

            "peek_hash_key_blok_response" ->
                PeekHashKblokResponse <$> aMessage .: "hashes"
-}
            "error"-> do
                msg <- aMessage .: "msg"
                errorCode   <- aMessage .: "errorCode"
                return $ StatusSyncMessage msg errorCode


            _ -> mzero

    parseJSON _ = mzero -- error $ show a

instance ToJSON MicroBlockContent where
  toJSON (MicroBlockContent aMicroblocksBD aTransactionsInfo) = object [
      "micro_block"   .= aMicroblocksBD,
      "transaction_info" .= aTransactionsInfo
    ]

instance FromJSON MicroBlockContent where
    parseJSON (Object mbc) = do
        aMicroblocksBD    <- mbc .: "micro_block"
        aTransactionsInfo <-  mbc .: "transaction_info"
        return $ MicroBlockContent aMicroblocksBD aTransactionsInfo
    parseJSON _ = mzero

--------------------------------------------------------------------------------
