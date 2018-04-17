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

import System.Clock
import Service.Network.UDP.Client
import Service.Metrics.Statsd

import Control.Monad
import Control.Concurrent.Chan

data InfoMsg = Metric String
             | Log String

sendLog, sendMetric :: String -> ClientHandle -> IO ()
sendLog = sendMetric
sendMetric stat h = sendAllTo (clientSocket h)
                              (encode stat)
                              (clientAddress h)


serveInfoMsg :: HostAddress -> PortNumber -> Chan InfoMsg -> Integer -> IO ()
serveInfoMsg host port chan aId = forever $ do
    m <- readChan chan
    case m of
        Metric s -> runClient host port $ sendMetric s
        Log s    -> do
            aTime <- getTime Realtime
            let aTag = "[" ++ show aId ++ "]["++ show aTime ++ "]"
            runClient host port $ sendLog $ aTag ++ s

--------------------------------------------------------------------------------
