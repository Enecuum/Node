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

import qualified Data.ByteString.Char8 as BS
import Network.Socket.ByteString (sendTo)
import Data.List

import System.Clock()
import Service.Network.Base
import Service.Network.TCP.Client
import Service.Metrics.Statsd

import Control.Monad (void, forever)
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
    | RegularTag
    | InitTag
  deriving (Show, Enum)


instance Show MsgType where
    show Info   = "info"
    show Warning = "warning"
    show Error  = "error"


data InfoMsg = Metric String | Log [LogingTag] MsgType String


sendToServer :: ClientHandle -> String -> IO ()
sendToServer h s = void $ sendTo (clientSocket h) (BS.pack s) (clientAddress h)

serveInfoMsg :: ConnectInfo -> ConnectInfo -> Chan InfoMsg -> String -> IO ()
serveInfoMsg statsdInfo logsInfo chan aId = do
    eithMHandler <- try (openConnect (host statsdInfo) (port statsdInfo))

    case eithMHandler of
      Left (err :: SomeException) -> putStrLn $ "Metrics server connection error: " ++ show err
      Right _                     -> putStrLn "Metrics server connected"

    eithLHandler <- try (openConnect (host logsInfo) (port logsInfo))

    case eithLHandler of
      Left (err :: SomeException) -> putStrLn $ "Logs server connection error: " ++ show err
      Right lHandler              -> do
            putStrLn "Logs server connected"
            sendToServer lHandler $ "+node|" ++  aId ++ "|" ++
                      intercalate "," (show <$> [ConnectingTag .. InitTag]) ++ "\r\n"

    forever $ do
        m <- readChan chan
        case m of
            Metric s -> case eithMHandler of
                          Left  _        -> return ()
                          Right mHandler -> sendToServer mHandler s

            Log aTags aMsgType aMsg -> do
                     let aTagsList = intercalate "," (show <$> aTags)

                         aString = "+log|" ++ aTagsList ++ "|" ++ aId  ++ "|"
                                   ++ show aMsgType ++  "|" ++ aMsg ++"\r\n"

                         aFileString = "  !  " ++ show aMsgType ++ "|" ++ aTagsList ++ "|" ++ aMsg ++"\n"
                     putStrLn aFileString
                     case eithLHandler of
                          Left  _        -> appendFile "log.txt" aFileString
                          Right lHandler -> sendToServer lHandler aString
