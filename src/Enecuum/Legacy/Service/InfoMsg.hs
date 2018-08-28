{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Enecuum.Legacy.Service.InfoMsg (
  withRate,
  increment,
  decrement,
  count,
  gauge,
  add,
  timing,
  set,
  serveInfoMsg
)  where

import qualified Data.ByteString.Char8                 as BS
import           Data.List
import           Network.Socket.ByteString             (sendTo)
import           Enecuum.Legacy.Node.BaseFunctions

import           Enecuum.Legacy.Service.Types                         ( InfoMsg(..), LoggingTag(..) )
import           Enecuum.Legacy.Service.Metrics.Statsd
import           Enecuum.Legacy.Service.Network.Base
import           Enecuum.Legacy.Service.Network.TCP.Client
import           System.Clock                          ()

import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Exception                     (SomeException, try)
import           Control.Monad                         (forever, void)


sendToServer :: ClientHandle -> String -> IO ()
sendToServer h s = void $ sendTo (clientSocket h) (BS.pack s) (clientAddress h)

serveInfoMsg :: ConnectInfo -> ConnectInfo -> OutChan InfoMsg -> String -> Bool -> IO ()
serveInfoMsg statsdInfo logsInfo chan aId stdout_log = do
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
                      intercalate "," (show <$> [ConnectingTag .. BDTag]) ++ "\r\n"

    undead (putStrLn "dead of log :))) ") $ forever $ do
        m <- readChan chan
        case m of
            Metric s -> case eithMHandler of
                          Left  _        -> return ()
                          Right mHandler -> sendToServer mHandler s

            Log aTags aMsgType aMsg -> do
                     let aTagsList = intercalate "," (show <$> aTags)

                         aString = "+log|" ++ aTagsList ++ "|" ++ aId  ++ "|"
                                   ++ show aMsgType ++  "|" ++ aMsg ++"\r\n"

                         aFileString = "  !  " ++ aId ++ "|" ++ show aMsgType ++ "|" ++ aTagsList ++ "|" ++ aMsg ++"\n"
                     appendFile "log.txt" aFileString
                     case eithLHandler of
                          Left  _        -> return ()
                          Right lHandler -> sendToServer lHandler aString
                     if stdout_log 
                     then putStrLn aFileString
                     else return ()
