{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Crypto.PubKey.ECC.ECDSA             as ECDSA
import           Data.Aeson
import qualified Data.ByteString                     as B
import qualified Data.ByteString.Lazy.Char8          as B8
import qualified Data.Text                           as T
import           Node.Data.Key
import           PoA.PoAServer
import           PoA.Types
import           Service.Network.Base
import           Service.Network.WebSockets.Client
import           Service.Types
import           Service.Types.PublicPrivateKeyPair
import           System.Environment                  (getArgs)

import qualified Network.WebSockets                  as WS
import           Service.Transaction.TransactionsDAG



testMsg = object [
    "msg" .= ("testMsg" :: String)
  ]

printBS bs = do
    putStrLn ""
    putStrLn . ("   " ++) . B8.unpack $ bs
    putStrLn ""


connectWithNN aStr aConnect = do
    putStrLn $ aStr ++ "Sending of hello request"
    sendMsg aConnect $ ActionConnect PoA Nothing

    aMsg <- receiveMsg aConnect
        (aStr ++ "Receiving of ID...")
        (aStr ++ "ID received.")

    aMyNodeId <- return $ case decode aMsg of
        Just (ResponseNodeId aId) -> aId
        Just _ -> error $ aStr ++ "FAIL. The received msg not a response for connect request! "
        _ -> error "FAIL. Error in the parsing!"
    putStrLn $ aStr ++ "received ID = " ++ show aMyNodeId
    return aMyNodeId


checkOfPending aConnect = do
    sendMsg aConnect $ RequestPending Nothing
    aPendingResonce <- receiveMsg aConnect
        "   Checking. Pending is empty?"
        "   Response from pendig received."

    ResponseTransactions aTransactions <- return $ case decode aPendingResonce of
        Just aTransactions@(ResponseTransactions _) -> aTransactions
        Just _ -> error "FAIL. The received msg not a response for pending request!"
        _ -> error "FAIL. Error in the parsing!"

    return aTransactions

receivingOfBroadcast aStr aConnect = do
    aMsg <- receiveMsg aConnect
        (aStr ++ "Receiving of broadcast echo...")
        (aStr ++ "Broadcast msg received.")

    MsgBroadcast (IdFrom aNodeId) _ aValue<- return $ case decode aMsg of
        Just aMsgBroadcast@(MsgBroadcast _ _ _) -> aMsgBroadcast
        Just _ -> error $ aStr ++ "FAIL. The received msg not a response for broadcast! "

    unless (aValue == testMsg) $ error $ aStr ++ "The broadcast msg is broaken."
    return aNodeId

sendMsg aConnect aMsg = do
    let aEncodedMsg = encode aMsg
    printBS aEncodedMsg
    WS.sendTextData aConnect aEncodedMsg

receiveMsg aConnect aStr1 aStr2 = do
    putStrLn aStr1
    aMsg <- WS.receiveData aConnect
    printBS aMsg
    putStrLn aStr2
    return aMsg

main = do
    aArgs <- getArgs
    case aArgs of
        "c":ip:_ -> do
            putStrLn ""
            putStrLn "--------------------------"
            putStrLn "   --------------------"
            putStrLn "   Start of test script"
            putStrLn "   --------------------"
            putStrLn "--------------------------"
            putStrLn ""
            aConnectListVar <- newEmptyMVar
            aSecondNodeIsStartedVar <-newEmptyMVar
            testsOk <- newEmptyMVar
            putStrLn "   Connecting to BN..."
            void . forkIO $ runClient ip 1554 "/" $ \aConnect -> do
                putStrLn "   Sending to BN reques for connects..."
                sendMsg aConnect $ RequestPotentialConnects False

                aMsg <- receiveMsg aConnect
                    "   Reciving from BN list of connects..."
                    "   Received list of NN from BN."

                aConnects <- return $ case decode aMsg of
                    Just (ResponsePotentialConnects aConnects) -> aConnects
                    Just _ -> error $ "   FAIL. The received msg not a list of connects! "
                    _ -> error "FAIL. Error in the parsing!"
                putMVar aConnectListVar aConnects

            aConnects <- takeMVar aConnectListVar
            putStrLn "   Testing firs NN of the list..."
            when (null aConnects) $ error "   FAIL. The received list is empty."
            putStrLn ""
            putStrLn "---------------------------------------"
            putStrLn "   ---------------------------------"
            putStrLn "   Resending and broadcasting of msg"
            putStrLn "   ---------------------------------"
            putStrLn "---------------------------------------"
            putStrLn ""
            aIdOfFirsClientVar <- newEmptyMVar
            putStrLn "1| Connecting CN to NN..."
            let (Connect aHostAddress port):(Connect aHostAddress2 port2):_ = aConnects
            putStrLn $ "1| Node adress: " ++ showHostAddress aHostAddress
            void . forkIO $ runClient (showHostAddress aHostAddress) (fromEnum port) "/" $ \aConnect -> do
                aMyNodeId <- connectWithNN "1| " aConnect
                putStrLn "1| CN is connected to NN."
                putMVar aIdOfFirsClientVar aMyNodeId

                aSecondNodeIsStarted <- takeMVar aSecondNodeIsStartedVar
                putStrLn "1| Sending of test broadcast msg..."
                sendMsg aConnect $ MsgBroadcast (IdFrom aMyNodeId) All testMsg
                aNodeId <- receivingOfBroadcast "1| " aConnect
                unless (aNodeId == aMyNodeId) $ error $ "1| The node ID en broadcast msg is broaken."

                aMsg <- receiveMsg aConnect
                    "1| Receiving msg from second node..."
                    "1| Received msg from second node."
                MsgMsgTo aNodeId _ aValue <- return $ case decode aMsg of
                    Just aMsgTo@(MsgMsgTo _ _ _) -> aMsgTo

                    a -> error $ "1| FAIL. The received msg not a correct!"
                unless (aValue == testMsg) $ error "1| The broadcast msg is broaken."

                putMVar testsOk True
                return ()

            aIdOfFirsClient <- takeMVar aIdOfFirsClientVar
            putStrLn "2| Connecting of CN to NN..."
            putStrLn $ "2| Node adress: " ++ showHostAddress aHostAddress2
            void . forkIO $ runClient (showHostAddress aHostAddress2) (fromEnum port2) "/" $ \aConnect -> do
                aMyId <- connectWithNN "2| " aConnect
                putStrLn "2| CN is connected to NN."
                putMVar aSecondNodeIsStartedVar True

                aNodeId <- receivingOfBroadcast "2| " aConnect

                putStrLn $ "2| Sending msg to firs node..."
                sendMsg aConnect $ MsgMsgTo (IdFrom aMyId)  (IdTo aNodeId) testMsg
                aMsg :: B.ByteString <- WS.receiveData aConnect
                return ()

            _ <- takeMVar testsOk
            putStrLn ""
            putStrLn "-------------"
            putStrLn "   -------"
            putStrLn "   Pending"
            putStrLn "   -------"
            putStrLn "-------------"
            putStrLn ""
            void . forkIO $ runClient (showHostAddress aHostAddress) (fromEnum port) "/" $ \aConnect -> do
                aTransaction:_ <- genNTx 10
                void $ connectWithNN "   " aConnect

                aTransactions <- checkOfPending aConnect
                unless (null aTransactions) $  error "  FAIL. Then pending not empty!"
                putStrLn "   Sending transaction in the pending."
                sendMsg aConnect $ MsgTransaction aTransaction
                aTransactions <- checkOfPending aConnect
                when (null aTransactions) $  error "   FAIL. Then pending is empty!"
                putStrLn "   Checking of pending clearing."
                sendMsg aConnect $ MsgMicroblock $ genMicroBlock aTransaction
                aTransactions <- checkOfPending aConnect
                unless (null aTransactions) $  error "   FAIL. Then pending not empty!"
                putMVar testsOk True

            _ <- takeMVar testsOk
            putStrLn ""
            putStrLn "-----------------"
            putStrLn "   -----------"
            putStrLn "   Testing Ok!"
            putStrLn "   -----------"
            putStrLn "-----------------"
            putStrLn ""

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
genMicroBlock tx = Microblock "123" (ECDSA.Signature 1 2) [] (PublicKey256k1 1) [tx]
