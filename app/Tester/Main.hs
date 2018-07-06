{-# LANGUAGE OverloadedStrings #-}
module Main where

import PoA.PoAServer
import Service.Network.WebSockets.Client
import System.Environment (getArgs)
import Control.Monad
import Data.Aeson
import Control.Concurrent.Async
import Control.Concurrent
import qualified Data.Text as T
import Service.Types

import qualified    Network.WebSockets                  as WS
import Service.Transaction.TransactionsDAG

data SendTransaction = SendTransaction Transaction | SendHello

instance ToJSON SendTransaction where
    toJSON (SendTransaction aTransaction) = object [
        "tag"           .= ("Request"  :: String),
        "type"          .= ("PendingAdd"  :: String),
        "transaction"   .= aTransaction
      ]
    toJSON SendHello = object [
        "tag"           .= ("Action"  :: String),
        "type"          .= ("Connect"  :: String),
        "node_type"     .= ("PoA" :: String)
      ]


main = do
    aArgs <- getArgs
    case aArgs of
        "b":ip:_ -> runClient ip 1554 "/" $ socketActor (\aConnect -> forever $ do
                threadDelay 1000
                WS.sendBinaryData aConnect ("{\"tag\": \"Request\", \"type\": \"Broadcast\", \"recipientType\" : \"PoA\", \"msg\" : { \"str\": \"000000000\"}}" :: T.Text))
        "t":ip:_ -> do
            aTransactions <- genNTx 1000
            putStrLn "Transactions generated"
            runClient ip 1554 "/" $ socketActor $ \aConnect ->
                forM_ (cycle aTransactions) $ \aTrans -> do
                    threadDelay 1000
                    WS.sendBinaryData aConnect $ encode $ SendTransaction aTrans
        _ -> return ()

socketActor aSender aConnect = do
    WS.sendTextData aConnect $ encode SendHello
    void $ race (aSender aConnect) (receiver 0)
  where
    receiver :: Int -> IO ()
    receiver i = do
        aMsg <- WS.receiveDataMessage aConnect
        print i
        print aMsg
        receiver $ i + 1
