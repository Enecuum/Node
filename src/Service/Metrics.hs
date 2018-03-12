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

  metric
)  where

import Network.Socket.ByteString (sendAllTo)
import Data.Serialize (encode)

import Service.Network.UDP.Client
import Service.Metrics.Statsd

import System.Environment (getEnv)

defaultHost :: IO String
defaultHost = getEnv "statsd"

defaultPort :: IO PortNumber
defaultPort = return 8125

sendMetric :: String -> ClientHandle -> IO ()
sendMetric stat h = sendAllTo (clientSocket h)
                              (encode stat)
                              (clientAddress h)

metric :: String -> IO ()
metric stat = do
         host <- defaultHost
         port <- defaultPort
         runClient host port (sendMetric stat)
