{-# LANGUAGE OverloadedStrings, LambdaCase, ScopedTypeVariables #-}

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
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Exception (try, SomeException)


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
    | InitTag
  deriving (Show, Enum)


instance Show MsgType where
    show Info   = "info"
    show Warning = "warning"
    show Error  = "error"


data InfoMsg = Metric String | Log [LogingTag] MsgType String


sendToServer :: ClientHandle -> String -> IO ()
sendToServer h s = void $ sendTo (clientSocket h) s (clientAddress h)

serveInfoMsg :: ConnectInfo -> ConnectInfo -> Chan InfoMsg -> Integer -> IO ()
serveInfoMsg statsdInfo logsInfo chan aId = do
    mCh <- newChan
    lCh <- newChan

    _ <- forkIO $ serveMetrics statsdInfo mCh
    _ <- forkIO $ serveLogs    logsInfo   lCh

    writeChan lCh $ "+node|" ++  show aId ++ "|" ++
          intercalate "," (show <$> [ConnectingTag .. InitTag]) ++ "\r\n"

    forever $ do
        m <- readChan chan
        case m of
            Metric s -> writeChan mCh s

            Log aTags aMsgType aMsg -> do
                let aTagsList = intercalate "," (show <$> aTags)

                    aString = "+log|" ++ aTagsList ++ "|" ++ show aId  ++ "|"
                        ++ show aMsgType ++  "|" ++ aMsg ++"\r\n"

                    aFileString = "  !  " ++ show aMsgType ++ "|" ++ aTagsList ++ "|" ++ aMsg ++"\n"

                appendFile "log.txt" aFileString
                writeChan lCh aString
--------------------------------------------------------------------------------

serveMetrics :: ConnectInfo -> Chan String -> IO ()
serveMetrics statsdInfo chan = do
  try (openConnect (host statsdInfo) (port statsdInfo)) >>= \case
    Left (err :: SomeException) -> do
      
      putStrLn $ "Metrics server connection error: " ++ show err
      loop
      where loop = do
              _ <- readChan chan
              loop

    Right metricHandle          -> do
      
      putStrLn "Metrics sever connected"
      loop
      where loop = do
              m <- readChan chan
              sendToServer metricHandle m 
              loop



serveLogs :: ConnectInfo -> Chan String -> IO ()
serveLogs logsInfo chan = do
  try (openConnect (host logsInfo) (port logsInfo)) >>= \case
    Left (err :: SomeException) -> do

      putStrLn $ "Logs server connection error: " ++ show err
      loop
      where loop = do
              _ <- readChan chan
              loop

    Right logsHandle            -> do

      putStrLn "Logs sever connected"
      loop
      where loop = do
              m <- readChan chan
              sendToServer logsHandle m
              loop

