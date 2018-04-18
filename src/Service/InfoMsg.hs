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
  InfoMsg(..),
  MsgType(..),
  LogingTag(..)
)  where

import Network.Socket.ByteString (sendAllTo)
import Data.Serialize (encode)
import Data.List

import System.Clock
import Service.Network.UDP.Client
import Service.Metrics.Statsd

import Control.Monad
import Control.Concurrent.Chan


--
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

sendLog, sendMetric :: String -> ClientHandle -> IO ()
sendLog = sendMetric
sendMetric stat h = sendAllTo (clientSocket h)
                              (encode stat)
                              (clientAddress h)

sendToLogServer a = undefined


serveInfoMsg :: HostAddress -> PortNumber -> Chan InfoMsg -> Integer -> IO ()
serveInfoMsg host port chan aId = do
    sendToLogServer $ "+node|" ++  show aId ++ "|" ++
        concat (intersperse "," (show <$> [
            ConnectingTag, LoadingShardsTag, BroadcatingTag, BootNodeTag,
            ShardingLvlTag, NetLvlTag, MiningLvlTag, ServePoATag])) ++ "\r\n"
    forever $ do
        m <- readChan chan
        case m of
            Metric s -> runClient host port $ sendMetric s
            Log aTags aMsgType aMsg -> do
                aTime <- getTime Realtime
                let aTagsList = concat (intersperse "," (show <$> aTags))
                    aString = "+log|" ++ aTagsList ++ "|" ++ show aId  ++ "|"
                        ++ show aMsgType ++  "|" ++ aMsg ++"\r\n"
                sendToLogServer aString
--------------------------------------------------------------------------------
