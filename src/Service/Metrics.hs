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

import Service.Config

defaultHost :: IO String
defaultHost = do
    maybeAddr <- getVar defaultConfig "Statsd" "addr"
    case maybeAddr of
      Nothing    -> return "127.0.0.1"
      Just addr  -> return addr

defaultPort :: IO PortNumber
defaultPort = do
    maybePort <- getVar defaultConfig "Statsd" "port"
    case maybePort of
      Nothing    -> return 8125
      Just port  -> return $ (read port)

sendMetric :: String -> ClientHandle -> IO ()
sendMetric stat h = sendAllTo (clientSocket h)
                              (encode stat)
                              (clientAddress h)

metric :: String -> IO ()
metric stat = do
         host <- defaultHost
         port <- defaultPort
         runClient host port (sendMetric stat)
