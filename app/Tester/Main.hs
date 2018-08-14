{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Crypto.PubKey.ECC.ECDSA           as ECDSA
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8        as B8
import qualified Data.Text                         as T
import           Node.Data.Key
import           Node.NetLvl.Messages
import           Service.Network.Base
import           Service.Network.WebSockets.Client
import           Service.Types
import           System.Environment                (getArgs)

import qualified Network.WebSockets                as WS
import           Service.System.Version
import           Service.Transaction.Common

testMsg :: Value
testMsg = object [
    "msg" .= ("testMsg" :: String)
  ]


printBS :: B8.ByteString -> IO ()
printBS bs = do
    putStrLn ""
    putStrLn . ("   " ++) . B8.unpack $ bs
    putStrLn ""


connectWithNN :: String -> NodeType -> WS.Connection -> IO NodeId
connectWithNN aStr aType aConnect = do
    putStrLn $ aStr ++ "Sending of hello request"
    sendMsg aConnect $ ActionConnect aType Nothing

    aMsg <- receiveMsg aConnect
        (aStr ++ "Receiving of ID...")
        (aStr ++ "ID received.")

    aMyNodeId <- return $ case decode aMsg of
        Just (ResponseNodeId aId) -> aId
        Just _ -> error $ aStr ++ "FAIL. The received msg not a response for connect request! "
        _ -> error "FAIL. Error in the parsing!"
    putStrLn $ aStr ++ "received ID = " ++ show aMyNodeId
    return aMyNodeId


connectHowNN :: String -> NodeId -> WS.Connection -> IO NodeId
connectHowNN  aStr aMyNodeId aConnect = do
    putStrLn $ aStr ++ "Sending of hello request"
    sendMsg aConnect $ ActionConnect NN (Just aMyNodeId)
    aMsg <- receiveMsg aConnect
        (aStr ++ "Receiving of ID...")
        (aStr ++ "ID received.")
    aNodeId <- return $ case decode aMsg of
        Just (ActionConnect NN (Just aNodeId)) -> aNodeId
        Just _ -> error $ aStr ++ "FAIL. The received msg not a response for connect request! "
        _ -> error "FAIL. Error in the parsing!"
    putStrLn $ aStr ++ "received ID = " ++ show aNodeId
    return aNodeId


checkOfPending :: WS.Connection -> IO [Transaction]
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

receivingOfBroadcast :: [Char] -> WS.Connection -> IO NodeId
receivingOfBroadcast aStr aConnect = do
    aMsg <- receiveMsg aConnect
        (aStr ++ "Receiving of broadcast...")
        (aStr ++ "Broadcast msg received.")

    MsgBroadcast (IdFrom aNodeId) _ aValue<- return $ case decode aMsg of
        Just aMsgBroadcast@(MsgBroadcast _ _ _) -> aMsgBroadcast
        Just _  -> error $ aStr ++ "FAIL. The received msg not a response for broadcast! "
        _       -> error "FAIL. Error in the parsing!"

    unless (aValue == testMsg) $ error $ aStr ++ "The broadcast msg is broaken."
    return aNodeId

sendMsg :: ToJSON a => WS.Connection -> a -> IO ()
sendMsg aConnect aMsg = do
    let aEncodedMsg = encode aMsg
    printBS aEncodedMsg
    WS.sendTextData aConnect aEncodedMsg

receiveMsg :: WS.Connection -> String -> String -> IO B8.ByteString
receiveMsg aConnect aStr1 aStr2 = do
    putStrLn aStr1
    aMsg <- WS.receiveData aConnect
    printBS aMsg
    putStrLn aStr2
    return aMsg

checkVersion :: WS.Connection -> IO ()
checkVersion aConnect = do
    putStrLn "   Sending version request..."
    sendMsg aConnect $ RequestVersion

    aMsg <- receiveMsg aConnect
        "   Receiving version response..."
        "   Received version response."

    aVersion <- return $ case decode aMsg of
        Just (ResponseVersion aVersion) -> aVersion
        Just _ -> error $ "   FAIL. The received msg not a version response! "
        _ -> error "FAIL. Error in the parsing!"
    when (aVersion /= $(version)) $ error "FAIL. Version of node /= version of tester!"


main :: IO ()
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
            let aWait = void $ takeMVar testsOk
            putStrLn "   Connecting to BN..."
            void . forkIO $ runClient ip 1554 "/" $ \aConnect -> do
                checkVersion aConnect
                putMVar testsOk True

            aWait
            void . forkIO $ runClient ip 1554 "/" $ \aConnect -> do
                putStrLn "   Sending to BN reques for connects..."
                sendMsg aConnect $ RequestPotentialConnects False

                aMsg <- receiveMsg aConnect
                    "   Receiving from BN list of connects..."
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
            let (Connect aHostAddress aPort):(Connect aHostAddress2 aPort2):_ = aConnects
            putStrLn $ "1| Node adress: " ++ showHostAddress aHostAddress
            void . forkIO $ runClient (showHostAddress aHostAddress) (fromEnum aPort) "/" $ \aConnect -> do
                aMyNodeId <- connectWithNN "1| " PoA aConnect
                putStrLn "1| CN is connected to NN."
                checkVersion aConnect
                putMVar aIdOfFirsClientVar aMyNodeId

                void $ takeMVar aSecondNodeIsStartedVar
                putStrLn "1| Sending of test broadcast msg..."
                sendMsg aConnect $ MsgBroadcast (IdFrom aMyNodeId) All testMsg
                aNodeId <- receivingOfBroadcast "1| " aConnect
                unless (aNodeId == aMyNodeId) $ error $ "1| The node ID en broadcast msg is broaken."

                aMsg <- receiveMsg aConnect
                    "1| Receiving msg from second node..."
                    "1| Received msg from second node."
                MsgMsgTo _ _ aValue <- return $ case decode aMsg of
                    Just aMsgTo@(MsgMsgTo _ _ _) -> aMsgTo

                    _ -> error $ "1| FAIL. The received msg not a correct!"
                unless (aValue == testMsg) $ error "1| The broadcast msg is broaken."

                putMVar testsOk True
                return ()

            void $ takeMVar aIdOfFirsClientVar
            putStrLn "2| Connecting of CN to NN..."
            putStrLn $ "2| Node adress: " ++ showHostAddress aHostAddress2
            void . forkIO $ runClient (showHostAddress aHostAddress2) (fromEnum aPort2) "/" $ \aConnect -> do
                aMyId <- connectWithNN "2| " PoA aConnect
                putStrLn "2| CN is connected to NN."
                checkVersion aConnect
                putMVar aSecondNodeIsStartedVar True

                aNodeId <- receivingOfBroadcast "2| " aConnect

                putStrLn $ "2| Sending msg to firs node..."
                sendMsg aConnect $ MsgMsgTo (IdFrom aMyId)  (IdTo aNodeId) testMsg

            aWait
            putStrLn ""
            putStrLn "-------------"
            putStrLn "   -------"
            putStrLn "   Pending"
            putStrLn "   -------"
            putStrLn "-------------"
            putStrLn ""
            void . forkIO $ runClient (showHostAddress aHostAddress) (fromEnum aPort) "/" $ \aConnect -> do
                aTransaction:_ <- genNTx 10
                void $ connectWithNN "   " PoA aConnect
                void $ do
                    aTransactions <- checkOfPending aConnect
                    unless (null aTransactions) $  error "  FAIL. Then pending not empty!"

                putStrLn "   Sending transaction in the pending."
                void $ do
                    sendMsg aConnect $ MsgTransaction aTransaction
                    aTransactions <- checkOfPending aConnect
                    when (null aTransactions) $  error "   FAIL. Then pending is empty!"

                putStrLn "   Checking of pending clearing."
                void $ do

                    sendMsg aConnect $ MsgMicroblock $ genMicroBlock aTransaction
                    aTransactions <- checkOfPending aConnect
                    unless (null aTransactions) $  error "   FAIL. Then pending not empty!"
                    putMVar testsOk True
            aWait
            putStrLn ""
            putStrLn "---------------------------------------"
            putStrLn "   ---------------------------------"
            putStrLn "   Resending (MicroBlocs, KeyBlocks)"
            putStrLn "   ---------------------------------"
            putStrLn "---------------------------------------"
            putStrLn ""
            putStrLn $ "1| Node adress: " ++ showHostAddress aHostAddress
            aNode1Start  <- newEmptyMVar
            void . forkIO $ runClient (showHostAddress aHostAddress) (fromEnum aPort) "/" $ \aConnect -> do
                void $ connectWithNN "1| " All aConnect
                putMVar aNode1Start True
                aMsg1 <- receiveMsg aConnect
                    ("1| " ++ "Receiving of msg Microblock...")
                    ("1| " ++ "Received msg Microblock.")
                void $ return $ case decode aMsg1 of
                    Just (MsgMicroblock _) -> 0 :: Int
                    _ -> error $ "1| FAIL. The received msg not a correct!"
                aMsg2 <- receiveMsg aConnect
                    ("1| " ++ "Receiving of msg KeyBlock...")
                    ("1| " ++ "Received msg KeyBlock.")
                void $ return $ case decode aMsg2 of
                    Just (MsgKeyBlock _) -> 0 :: Int
                    _ -> error $ "1| FAIL. The received msg not a correct!"
                putMVar testsOk True

            _ <- takeMVar aNode1Start
            putStrLn $ "2| Node adress: " ++ showHostAddress aHostAddress2
            void . forkIO $ runClient (showHostAddress aHostAddress2) (fromEnum aPort2) "/" $ \aConnect -> do
                void $ connectWithNN "2| " PoA aConnect
                aTransaction:_ <- genNTx 10
                putStrLn "2| Sending of Microblock"
                sendMsg aConnect $ MsgMicroblock $ genMicroBlock aTransaction
                putStrLn "2| Sending of KeyBlock"
                sendMsg aConnect $ MsgKeyBlock testMsg

            aWait
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
        "l":ip:aFile:_ -> do
            aTrafic <- readFile aFile
            runClient ip 1554 "/" $ socketActor $ \aConnect ->
                forM_ (lines aTrafic) $ \aMsg -> do
                    threadDelay 50000
                    WS.sendTextData aConnect $ B8.pack aMsg
        _ -> return ()


socketActor :: (WS.Connection -> IO a) -> WS.Connection -> IO ()
socketActor aSender aConnect = do
    WS.sendTextData aConnect $ encode (ActionConnect All Nothing)
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
