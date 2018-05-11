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
    | ServerBootNodeTag
    | GCTag
    | RegularTag
    | InitTag
  deriving (Show, Enum)


instance Show MsgType where
    show Info   = "info"
    show Warning = "warning"
    show Error  = "error"


data InfoMsg = Metric String | Log [LogingTag] MsgType String


sendToServer :: ClientHandle -> String -> IO ()
sendToServer h s = void $ sendTo (clientSocket h) s (clientAddress h)

serveInfoMsg :: ConnectInfo -> ConnectInfo -> Chan InfoMsg -> String -> IO ()
serveInfoMsg statsdInfo logsInfo chan aId = do
    putStrLn "Start of serveInfoMsg"
    --metricHandle <- openConnect (host statsdInfo) (port statsdInfo)
--    putStrLn "Metrics server connected"
    logHandle    <- openConnect (host logsInfo)   (port logsInfo)
    putStrLn "Logs server connected"

    sendToServer logHandle $ "+node|" ++  aId ++ "|" ++
          intercalate "," (show <$> [ConnectingTag .. InitTag]) ++ "\r\n"

    forever $ do
        m <- readChan chan
        case m of
            Metric s -> return ()-- sendToServer metricHandle s

            Log aTags aMsgType aMsg -> do
                let aTagsList = intercalate "," (show <$> aTags)

                    aString = "+log|" ++ aTagsList ++ "|" ++ aId  ++ "|"
                        ++ show aMsgType ++  "|" ++ aMsg ++"\r\n"

                    aFileString = "  !  " ++ show aMsgType ++ "|" ++ aTagsList ++ "|" ++ aMsg ++"\n"

                putStrLn aFileString
                sendToServer logHandle aString
--------------------------------------------------------------------------------
