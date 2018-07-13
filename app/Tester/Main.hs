{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Main where

import PoA.PoAServer
import PoA.Types
import Service.Network.WebSockets.Client
import System.Environment (getArgs)
import Control.Monad
import Data.Aeson
import Control.Concurrent.Async
import Control.Concurrent
import qualified Data.ByteString    as B
import qualified Data.ByteString.Lazy.Char8    as B8
import qualified Data.Text          as T
import Service.Types
import Node.Data.Key
import Service.Network.Base
import Service.Types.PublicPrivateKeyPair
import Crypto.PubKey.ECC.ECDSA as ECDSA

import qualified    Network.WebSockets                  as WS
import Service.Transaction.TransactionsDAG


testMsg = object [
    "msg" .= ("testMsg" :: String)
  ]

printBS bs = do
    putStrLn ""
    putStrLn . ("   " ++) . B8.unpack $ bs
    putStrLn ""


connectWithNN aStr aConnect = do
    putStrLn $ aStr ++ "Sending of hello request"
    let aConnectMsg = encode $ ActionConnect PoA Nothing
    printBS aConnectMsg
    WS.sendTextData aConnect aConnectMsg

    putStrLn $ aStr ++ "Receiving of ID."
    aMsg <- WS.receiveData aConnect
    printBS aMsg
    aMyNodeId <- return $ case decode aMsg of
        Just (ResponseNodeId aId) -> aId
        _ -> error $
            aStr ++ "FAIL. The received msg not a response for connect request! "
    putStrLn $ aStr ++ "received ID = " ++ show aMyNodeId
    return aMyNodeId


checkOfPending aConnect = do
    let aRequetPending  = encode $ RequestPending Nothing
    printBS aRequetPending
    WS.sendTextData aConnect aRequetPending
    putStrLn $ "   Checking. Pending is empty?"
    aPendingResonce <- WS.receiveData aConnect
    printBS aPendingResonce
    ResponseTransactions aTransactions <- return $ case decode aPendingResonce of
        Just aTransactions@(ResponseTransactions _) -> aTransactions
        Just _ -> error "  FAIL. The received msg not a response for pending request!"
        _ -> error "  FAIL. Error in the parsing!"
    return aTransactions

main = do
    aArgs <- getArgs
    case aArgs of
        "c":ip:_ -> do
            putStrLn "   --------------------"
            putStrLn "   Start of test script"
            putStrLn "   --------------------"
            aConnectListVar <- newEmptyMVar
            aSecondNodeIsStartedVar <-newEmptyMVar
            testsOk <- newEmptyMVar
            putStrLn "   Connecting to BN..."
            void . forkIO $ runClient ip 1554 "/" $ \aConnect -> do
                putStrLn "   Sending to BN reques for connects..."
                let aRequestPotentialConnects = encode $ RequestPotentialConnects False
                WS.sendTextData aConnect aRequestPotentialConnects
                printBS aRequestPotentialConnects
                putStrLn "   Reciving from BN list of connects..."
                aMsg <- WS.receiveData aConnect
                printBS aMsg
                let aConnects = case decode aMsg of
                        Just (ResponsePotentialConnects aConnects) -> aConnects
                        _ -> error $
                            "   FAIL. The received msg not a list of connects! "
                putMVar aConnectListVar aConnects
            aConnects <- takeMVar aConnectListVar
            putStrLn "   Received list of NN from BN."

            putStrLn "   Testing firs NN of the list..."
            when (null aConnects) $ error "   FAIL. The received list is empty."
            let (Connect aHostAddress port):_ = aConnects
            putStrLn "   ---------------------------------"
            putStrLn "   Resending and broadcasting of msg"
            putStrLn "   ---------------------------------"
            aIdOfFirsClientVar <- newEmptyMVar
            putStrLn "1| Connecting CN to NN..."
            void . forkIO $ runClient (showHostAddress aHostAddress) (fromEnum port) "/" $ \aConnect -> do
                aMyNodeId <- connectWithNN "1| " aConnect
                putStrLn "1| CN is connected to NN."
                putMVar aIdOfFirsClientVar aMyNodeId

                aSecondNodeIsStarted <- takeMVar aSecondNodeIsStartedVar
                putStrLn "1| Sending of test broadcast msg..."
                let aBroadcastMsg = encode $ MsgBroadcast (IdFrom aMyNodeId) All testMsg
                printBS aBroadcastMsg
                WS.sendTextData aConnect aBroadcastMsg
                putStrLn "1| Receiving of broadcast echo..."
                aMsg <- WS.receiveData aConnect
                printBS aMsg
                MsgBroadcast (IdFrom aNodeId) _ aValue<- return $ case decode aMsg of
                    Just aMsgBroadcast@(MsgBroadcast _ _ _) -> aMsgBroadcast
                    _ -> error $
                        "1| FAIL. The received msg not a response for broadcast! "
                putStrLn "1| Broadcast echo received."
                unless (aNodeId == aMyNodeId) $ error "1| The node ID en broadcast msg is broaken."
                unless (aValue == testMsg)    $ error "1| The broadcast msg is broaken."

                aMsg <- WS.receiveData aConnect
                printBS aMsg
                putStrLn "1| Received msg from second node."
                MsgMsgTo aNodeId _ aValue <- return $ case decode aMsg of
                    Just aMsgTo@(MsgMsgTo _ _ _) -> aMsgTo
                    Nothing -> error $
                        "1| FAIL. The received msg not a correct! "
                unless (aValue == testMsg) $ error "1| The broadcast msg is broaken."

                putMVar testsOk True
                return ()

            aIdOfFirsClient <- takeMVar aIdOfFirsClientVar
            putStrLn "2| Connecting of CN to NN..."
            void . forkIO $ runClient (showHostAddress aHostAddress) (fromEnum port) "/" $ \aConnect -> do
                aMyId <- connectWithNN "2| " aConnect
                putStrLn "2| CN is connected to NN."
                putMVar aSecondNodeIsStartedVar True

                aMsg <- WS.receiveData aConnect
                printBS aMsg
                putStrLn "2| Broadcast msg received."
                MsgBroadcast (IdFrom aNodeId) _ aValue<- return $ case decode aMsg of
                    Just aMsgBroadcast@(MsgBroadcast _ _ _) -> aMsgBroadcast
                    _ -> error $
                        "2| FAIL. The received msg not a response for broadcast! "
                unless (aValue == testMsg)    $ error "2| The broadcast msg is broaken."
                putStrLn $ "2| Sending msg to firs node..."
                let aMsgTo = encode $ MsgMsgTo (IdFrom aMyId)  (IdTo aNodeId) testMsg
                printBS aMsgTo
                WS.sendTextData aConnect aMsgTo
                aMsg :: B.ByteString <- WS.receiveData aConnect
                return ()

            _ <- takeMVar testsOk
            putStrLn "   -------"
            putStrLn "   Pending"
            putStrLn "   -------"

            void . forkIO $ runClient (showHostAddress aHostAddress) (fromEnum port) "/" $ \aConnect -> do
                aTransaction:_ <- genNTx 10
                void $ connectWithNN "   " aConnect
                let aMsgTransaction = encode $ MsgTransaction aTransaction
                    aMsgMicroblock  = encode $ MsgMicroblock $ genMicroBlock aTransaction
                    aRequetPending  = encode $ RequestPending Nothing


                aTransactions <- checkOfPending aConnect
                unless (null aTransactions) $  error "  FAIL. Then pending not empty!"

                putStrLn "Sending transaction in the pending."
                printBS aMsgTransaction
                WS.sendTextData aConnect aMsgTransaction


                aTransactions <- checkOfPending aConnect
                when (null aTransactions) $  error "   FAIL. Then pending is empty!"

                putStrLn "Checking of pending clearing."
                WS.sendTextData aConnect aMsgMicroblock
                printBS aMsgMicroblock

                aTransactions <- checkOfPending aConnect
                unless (null aTransactions) $  error "   FAIL. Then pending not empty!"
                putMVar testsOk True

            _ <- takeMVar testsOk
            putStrLn "   -----------"
            putStrLn "   Testing Ok!"
            putStrLn "   -----------"


        "b":ip:_ -> runClient ip 1554 "/" $ socketActor (\aConnect -> forever $ do
                threadDelay 1000
                WS.sendBinaryData aConnect ("{\"tag\": \"Request\", \"type\": \"Broadcast\", \"recipientType\" : \"PoA\", \"msg\" : { \"str\": \"000000000\"}}" :: T.Text))
        "t":ip:_ -> do
            aTransactions <- genNTx 1000
            putStrLn "Transactions generated"
            runClient ip 1554 "/" $ socketActor $ \aConnect ->
                forM_ (cycle aTransactions) $ \aTrans -> do
                    threadDelay 1000
                    WS.sendBinaryData aConnect $ encode $ MsgTransaction aTrans
        _ -> return ()

socketActor aSender aConnect = do
    WS.sendTextData aConnect $ encode (ActionConnect PoA Nothing)
    void $ race (aSender aConnect) (receiver 0)
  where
    receiver :: Int -> IO ()
    receiver i = do
        aMsg <- WS.receiveDataMessage aConnect
        print i
        print aMsg
        receiver $ i + 1


--
genMicroBlock :: Transaction -> Microblock
genMicroBlock tx = Microblock "123" (ECDSA.Signature 1 2) [] (PublicKey256k1 1) [tx] 1
