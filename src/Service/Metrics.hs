{-# LANGUAGE OverloadedStrings #-}

module Service.Metrics (
  withRate,
  increment,
  decrement,
  count,
  gauge,
  add,
  timing,
  set,

  serveMetrics,
  Metric
)  where

import Network.Socket.ByteString (sendAllTo)
import Data.Serialize (encode)

import Service.Network.UDP.Client
import Service.Metrics.Statsd

import Control.Concurrent.Chan

type Metric = String

sendMetric :: String -> ClientHandle -> IO ()
sendMetric stat h = sendAllTo (clientSocket h)
                              (encode stat)
                              (clientAddress h)

serveMetrics :: HostAddress -> PortNumber -> Chan Metric -> IO ()
serveMetrics host port chan = do
             m <- readChan chan
             runClient host port $ sendMetric m
               
