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

data InfoMsg = Metric String
             | Log String

sendMetric :: String -> ClientHandle -> IO ()
sendMetric stat h = sendAllTo (clientSocket h)
                              (encode stat)
                              (clientAddress h)

serveInfoMsg :: HostAddress -> PortNumber -> Chan InfoMsg -> IO ()
serveInfoMsg host port chan = do
             m <- readChan chan
             case m of
               Metric s -> runClient host port $ sendMetric s
               Log s    -> undefined
               
