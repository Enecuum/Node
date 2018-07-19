{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PackageImports           #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE DeriveGeneric            #-}

module Service.Sync.SyncJson where

import           Data.Aeson
import           Service.Types
import           Service.Transaction.LedgerSync
import qualified Data.ByteString.Internal              as BSI
import qualified Data.ByteString.Char8                 as BS
import qualified Data.Text                             as T
import           GHC.Generics (Generic)
import           Control.Monad
--type HashOfKeyBlock = ByteString
type HashOfMicroblock = BSI.ByteString
data MicroBlokContent = MicroBlokContent [MicroblockBD] [TransactionInfo] deriving (Show, Read, Generic)

type LastNumber = Int
type Count      = Int
type SyncStatusMessage  = String
type ErrorStringCode    = String

data SyncEvent where
    RestartSync ::                SyncEvent
    SyncMsg     :: SyncMessage -> SyncEvent

data SyncMessage where
    RequestTail           ::                                             SyncMessage
    ResponseTail          :: (Number, HashOfKeyBlock)                 -> SyncMessage
    PeekHashKblokRequest  :: From             -> To                   -> SyncMessage
    PeekHashKblokResponse :: [(Number, HashOfKeyBlock)]               -> SyncMessage
    PeekKeyBlokRequest    :: From             -> To                   -> SyncMessage
    PeekKeyBlokResponse   :: [(Number, HashOfKeyBlock, MacroblockBD)] -> SyncMessage
    MicroblockRequest     :: HashOfMicroblock                         -> SyncMessage
    MicroblockResponse    :: MicroBlokContent                         -> SyncMessage
    StatusSyncMessage     :: SyncStatusMessage -> ErrorStringCode     -> SyncMessage
  deriving (Show)



instance ToJSON SyncMessage where
    toJSON RequestTail = object [
        "sync"      .= ("tail_request"   :: String)
      ]

    toJSON (ResponseTail (aLastNumber, aHashOfKeyBlock)) = object [
        "sync"        .= ("tail_response"   :: String),
        "last_number" .= aLastNumber,
        "last_hash"   .= BS.unpack aHashOfKeyBlock
      ]

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
    toJSON (StatusSyncMessage msg errorCode) = object [
        "sync"      .= ("error"   :: String),
        "msg"       .= msg,
        "errorCode" .= errorCode
      ]

instance FromJSON SyncMessage where
    parseJSON (Object aMessage) = do
        sync  :: T.Text <- aMessage .:"sync"
        case (T.unpack sync) of
            "tail_request" -> return RequestTail

            "tail_response"-> do
                lastNumber <- aMessage .: "last_number"
                lastHash   <- aMessage .: "last_hash"
                return $ ResponseTail (lastNumber, lastHash)

            "peek_key_blok_request"-> do
                from <- aMessage .: "from"
                to   <- aMessage .: "to"
                return $ PeekKeyBlokRequest from to

            "peek_key_blok_response"->
                PeekKeyBlokResponse <$> aMessage .: "kblocks"

            "block_mickro_blok_request"->
                MicroblockRequest <$> aMessage .: "block_hash"

            "peek_hash_key_blok_response"->
                MicroblockResponse <$> aMessage .: "hashes"

            "peek_hash_key_blok_request"-> do
                aCount <- aMessage .: "kblock_count"
                lastHash   <- aMessage .: "block_hash"
                return $ PeekHashKblokRequest aCount lastHash

            "peek_hash_key_blok_response" ->
                PeekHashKblokResponse <$> aMessage .: "hashes"

            "error"-> do
                msg <- aMessage .: "msg"
                errorCode   <- aMessage .: "error"
                return $ StatusSyncMessage msg errorCode


            _ -> mzero

    parseJSON _ = mzero -- error $ show a

instance ToJSON MicroBlokContent where
  toJSON (MicroBlokContent aMicroblocksBD aTransactionsInfo) = object [
      "micro_block"   .= aMicroblocksBD,
      "transaction_info" .= aTransactionsInfo
    ]

instance FromJSON MicroBlokContent where
    parseJSON (Object mbc) = do
        aMicroblocksBD    <- mbc .: "micro_block"
        aTransactionsInfo <-  mbc .: "transaction_info"
        return $ MicroBlokContent aMicroblocksBD aTransactionsInfo
    parseJSON _ = mzero

--------------------------------------------------------------------------------
