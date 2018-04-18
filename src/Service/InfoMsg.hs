{-# LANGUAGE OverloadedStrings #-}

module Service.InfoMsg (
  withRate,
  increment,
  decrement,
  count,
  gauge,
  add,
  timing,
  set,

  serveInfoMsg,
  InfoMsg(..)
)  where

import Network.Socket.ByteString (sendAllTo)
import Data.Serialize (encode)

import Service.Network.UDP.Client
import Service.Metrics.Statsd

import Control.Concurrent.Chan

data MsgType = Info | Warnig | Error

data LogingTag
    = ConnectingTag
    | LoadingShardsTag
    | BroadcatingTag
    | BootNodeTag
    | ShardingLvlTag
    | NetLvlTag
    | MiningLvlTag
    | ServePoATag
  deriving Show


instance Show MsgType where
    show Info   = "info"
    show Warnig = "warnig"
    show Error  = "error"


data InfoMsg = Metric String
             | Log [LogingTag] MsgType String


sendToServer :: ClientHandle -> String -> IO ()
sendToServer h s = sendAllTo (clientSocket h)
                             (encode s)
                             (clientAddress h)

serveInfoMsg :: ConnectInfo -> ConnectInfo -> Chan InfoMsg -> Integer -> IO ()
serveInfoMsg statsdInfo logsInfo  chan aId = do
    metricHandle <- openConnect (host statsdInfo) (port statsdInfo)
    logHandle    <- openConnect (host logsInfo)   (port logsInfo)   

    sendToServer logHandle $ "+node|" ++  show aId ++ "|" ++
        concat (intersperse "," (show <$> [
            ConnectingTag, LoadingShardsTag, BroadcatingTag, BootNodeTag,
            ShardingLvlTag, NetLvlTag, MiningLvlTag, ServePoATag])) ++ "\r\n"

    forever $ do
        m <- readChan chan
        case m of
            Metric s -> sendToServer metricHandle s
            Log aTags aMsgType aMsg -> do
                aTime <- getTime Realtime
                let aTagsList = concat (intersperse "," (show <$> aTags))
                    aString = "+log|" ++ aTagsList ++ "|" ++ show aId  ++ "|"
                        ++ show aMsgType ++  "|" ++ aMsg ++"\r\n"
                sendToServer logHandle aString
--------------------------------------------------------------------------------
