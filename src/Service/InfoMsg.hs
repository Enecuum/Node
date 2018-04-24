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

import Network.Socket (sendTo)
import Data.List

import System.Clock()
import Service.Network.Base
import Service.Network.TCP.Client
import qualified Service.Network.UDP.Client as UDP
import Service.Metrics.Statsd

import Control.Monad (void, forever)
import Control.Concurrent.Chan


data MsgType = Info | Warning | Error

data LogingTag
    = ConnectingTag
    | LoadingShardsTag
    | BroadcatingTag
    | BootNodeTag
    | ShardingLvlTag
    | NetLvlTag
    | MiningLvlTag
    | ServePoATag
    | GCTag
    | InitTag
  deriving Show


instance Show MsgType where
    show Info   = "info"
    show Warning = "warning"
    show Error  = "error"


data InfoMsg = Metric String
             | Log [LogingTag] MsgType String



sendToServer :: ClientHandle -> String -> IO ()
sendToServer h s = void $ sendTo (clientSocket h)
                                 s
                                (clientAddress h)

serveInfoMsg :: ConnectInfo -> ConnectInfo -> Chan InfoMsg -> Integer -> IO ()
serveInfoMsg statsdInfo logsInfo chan aId = do
    metricHandle <- UDP.openConnect (host statsdInfo) (port statsdInfo)
    putStrLn "Metrics connected"
    logHandle    <- openConnect (host logsInfo)   (port logsInfo)
    putStrLn "Logs connected"
    sendToServer logHandle $ "+node|" ++  show aId ++ "|" ++
          concat (intersperse "," (show <$> [
            ConnectingTag, LoadingShardsTag, BroadcatingTag, BootNodeTag,
            ShardingLvlTag, NetLvlTag, MiningLvlTag, ServePoATag])) ++ "\r\n"

    forever $ do
        m <- readChan chan
        case m of
            Metric s -> sendToServer metricHandle s

            Log aTags aMsgType aMsg -> do
                let aTagsList = concat (intersperse "," (show <$> aTags))

                    aString = "+log|" ++ aTagsList ++ "|" ++ show aId  ++ "|"
                        ++ show aMsgType ++  "|" ++ aMsg ++"\r\n"

                    aFileString = "  !  " ++ show aMsgType ++ "|" ++ aTagsList ++ "|" ++ aMsg ++"\n"

                appendFile "log.txt" aFileString
                putStrLn aString
                sendToServer logHandle aString
--------------------------------------------------------------------------------
