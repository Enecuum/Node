{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PackageImports           #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}

module Enecuum.Legacy.Service.Sync.SyncJson where

import           Control.Exception
import           Control.Monad
import           Data.Aeson
import qualified Data.Text                                  as T
import           Enecuum.Legacy.Node.Data.Key
import           Enecuum.Legacy.Service.Sync.SyncTypes
import           Enecuum.Legacy.Service.Transaction.Storage ()
import           Enecuum.Legacy.Service.Types
import           Enecuum.Prelude


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
    StatusSyncMessage     :: SyncStatusMessage -> ErrorStringCode     -> SyncMessage
  deriving (Show)


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
    toJSON (ResponseChain aChunk) = object [
        "sync"   .= ("chunk"  :: String),
        "chunk"  .= aChunk
      ]
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
    parseJSON v = throw $ DecodeException $ "Can not parse chunk " ++ show v

instance ToJSON Chunk where
    toJSON (Chunk aKey aMBs) = object [
        "block"       .= aKey,
        "microblocks" .= aMBs
      ]


instance FromJSON SyncMessage where
    parseJSON (Object aMessage) = do
        sync  :: T.Text <- aMessage .:"sync"
        case (T.unpack sync) of
            "tail" -> return RequestTail
            "peak" -> ResponseTail <$> aMessage .: "tail"
            "sync" -> return RequestChain
            "chunk"-> ResponseChain <$> aMessage .: "chunk"

            "error"-> do
                msg <- aMessage .: "msg"
                errorCode   <- aMessage .: "errorCode"
                return $ StatusSyncMessage msg errorCode


            _ -> mzero

    parseJSON _ = mzero

instance ToJSON MicroBlockContent where
  toJSON (MicroBlockContent aMicroblocks) = object [
      "micro_block"   .= aMicroblocks
    ]

instance FromJSON MicroBlockContent where
    parseJSON (Object mbc) = do
        aMicroblocks    <- mbc .: "micro_block"
        return $ MicroBlockContent aMicroblocks
    parseJSON _ = mzero

--------------------------------------------------------------------------------
