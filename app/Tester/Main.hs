{-# LANGUAGE OverloadedStrings #-}
module Main where

import PoA.PoAServer
import Service.Network.WebSockets.Client
import System.Environment (getArgs)
import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent
import qualified Data.Text as T

import qualified    Network.WebSockets                  as WS



main = do
    aArgs <- getArgs
    if null aArgs then print "Add ip to args!!!"
    else do
        let ip = head aArgs
        runClient ip 1554 "/" socketActor


socketActor :: WS.Connection -> IO ()
socketActor aConnect = do
    WS.sendTextData aConnect ("{\"tag\":\"Response\",\"type\": \"NodeId\",\"nodeId\": \"31111111111111111111111111111113\",\"nodeType\" : \"PoA\" }" :: T.Text)
    void $ race sender (receiver 0)
  where
    sender :: IO ()
    sender = forever $ do
        threadDelay 1000
        WS.sendBinaryData aConnect ("{\"tag\": \"Request\", \"type\": \"Broadcast\", \"recipientType\" : \"PoA\", \"msg\" : { \"str\": \"000000000\"}}" :: T.Text)

    receiver :: Int -> IO ()
    receiver i = do
        void $ WS.receiveDataMessage aConnect
        print i
        receiver $ i + 1
