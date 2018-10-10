{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main where

import           Control.Concurrent                               (forkIO,
                                                                   threadDelay)
import           Control.Concurrent.Async
import           Crypto.PubKey.ECC.ECDSA                          as ECDSA
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8                       as B8
import qualified Data.Text                                        as T
import           Enecuum.Legacy.Node.Data.Key
import           Enecuum.Legacy.Node.NetLvl.Messages
import           Enecuum.Legacy.Service.Network.Base
import           Enecuum.Legacy.Service.Network.WebSockets.Client
import           Enecuum.Legacy.Service.System.Version
import           Enecuum.Legacy.Service.Transaction.Common
import           Enecuum.Legacy.Service.Types
import           Enecuum.Prelude                                  hiding ((.=))
import qualified Network.WebSockets                               as WS


testMsg :: Value
testMsg = object ["msg" .= ("testMsg" :: String)]


printBS :: B8.ByteString -> IO ()
printBS bs = do
    print ""
    print . ("   " ++) . B8.unpack $ bs
    print ""


connectWithNN :: String -> NodeType -> WS.Connection -> IO NodeId
connectWithNN aStr aType aConnect = do
    print $ aStr ++ "Sending of hello request"
    sendMsg aConnect $ ActionConnect aType Nothing

    aMsg      <- receiveMsg aConnect (aStr ++ "Receiving of ID...") (aStr ++ "ID received.")

    aMyNodeId <- pure $ case decode aMsg of
        Just (ResponseNodeId aId) -> aId
        Just _ -> error $ T.pack $ aStr ++ "FAIL. The received msg not a response for connect request! "
        _ -> error "FAIL. Error in the parsing!"
    print $ aStr ++ "received ID = " ++ show aMyNodeId
    pure aMyNodeId


connectHowNN :: String -> NodeId -> WS.Connection -> IO NodeId
connectHowNN aStr aMyNodeId aConnect = do
    print $ aStr ++ "Sending of hello request"
    sendMsg aConnect $ ActionConnect NN (Just aMyNodeId)
    aMsg    <- receiveMsg aConnect (aStr ++ "Receiving of ID...") (aStr ++ "ID received.")
    aNodeId <- pure $ case decode aMsg of
        Just (ActionConnect NN (Just aNodeId)) -> aNodeId
        Just _ -> error $ T.pack $ aStr ++ "FAIL. The received msg not a response for connect request! "
        _      -> error "FAIL. Error in the parsing!"
    print $ aStr ++ "received ID = " ++ show aNodeId
    pure aNodeId


checkOfPending :: WS.Connection -> IO [Transaction]
checkOfPending aConnect = do
    sendMsg aConnect $ RequestPending Nothing
    aPendingResonce <- receiveMsg aConnect "   Checking. Pending is empty?" "   Response from pendig received."

    ResponseTransactions aTransactions <- pure $ case decode aPendingResonce of
        Just aTransactions@(ResponseTransactions _) -> aTransactions
        Just _ -> error "FAIL. The received msg not a response for pending request!"
        _      -> error "FAIL. Error in the parsing!"

    pure aTransactions

receivingOfBroadcast :: [Char] -> WS.Connection -> IO NodeId
receivingOfBroadcast aStr aConnect = do
    aMsg <- receiveMsg aConnect (aStr ++ "Receiving of broadcast...") (aStr ++ "Broadcast msg received.")

    MsgBroadcast (IdFrom aNodeId) _ aValue <- pure $ case decode aMsg of
        Just aMsgBroadcast@(MsgBroadcast _ _ _) -> aMsgBroadcast
        Just _ -> error $ T.pack $ aStr ++ "FAIL. The received msg not a response for broadcast! "
        _      -> error "FAIL. Error in the parsing!"

    unless (aValue == testMsg) $ error $ T.pack $ aStr ++ "The broadcast msg is broaken."
    pure aNodeId

sendMsg :: ToJSON a => WS.Connection -> a -> IO ()
sendMsg aConnect aMsg = do
    let aEncodedMsg = encode aMsg
    printBS aEncodedMsg
    WS.sendTextData aConnect aEncodedMsg

receiveMsg :: WS.Connection -> String -> String -> IO B8.ByteString
receiveMsg aConnect aStr1 aStr2 = do
    print aStr1
    aMsg <- WS.receiveData aConnect
    printBS aMsg
    print aStr2
    pure aMsg

checkVersion :: WS.Connection -> IO ()
checkVersion aConnect = do
    print "   Sending version request..."
    sendMsg aConnect $ RequestVersion

    aMsg     <- receiveMsg aConnect "   Receiving version response..." "   Received version response."

    aVersion <- pure $ case decode aMsg of
        Just (ResponseVersion aVersion) -> aVersion
        Just _                          -> error $ "   FAIL. The received msg not a version response! "
        _                               -> error "FAIL. Error in the parsing!"
    when (aVersion /= $(version)) $ error "FAIL. Version of node /= version of tester!"


main :: IO ()
main = do
    aArgs <- getArgs
    case aArgs of
        "c" : ip : _ -> do
            print ""
            print "--------------------------"
            print "   --------------------"
            print "   Start of test script"
            print "   --------------------"
            print "--------------------------"
            print ""
            aConnectListVar         <- newEmptyMVar
            aSecondNodeIsStartedVar <- newEmptyMVar
            testsOk                 <- newEmptyMVar
            let aWait = void $ takeMVar testsOk
            print "   Connecting to BN..."
            void . forkIO $ runClient ip 1554 "/" $ \aConnect -> do
                checkVersion aConnect
                putMVar testsOk True

            aWait
            void . forkIO $ runClient ip 1554 "/" $ \aConnect -> do
                print "   Sending to BN reques for connects..."
                sendMsg aConnect $ RequestPotentialConnects False

                aMsg <- receiveMsg aConnect "   Receiving from BN list of connects..." "   Received list of NN from BN."

                aConnects <- pure $ case decode aMsg of
                    Just (ResponsePotentialConnects aConnects) -> aConnects
                    Just _ -> error $ "   FAIL. The received msg not a list of connects! "
                    _      -> error "FAIL. Error in the parsing!"
                putMVar aConnectListVar aConnects

            aConnects <- takeMVar aConnectListVar
            print "   Testing firs NN of the list..."
            when (null aConnects) $ error "   FAIL. The received list is empty."
            print ""
            print "---------------------------------------"
            print "   ---------------------------------"
            print "   Resending and broadcasting of msg"
            print "   ---------------------------------"
            print "---------------------------------------"
            print ""
            aIdOfFirsClientVar <- newEmptyMVar
            print "1| Connecting CN to NN..."
            let (Connect aHostAddress aPort) : (Connect aHostAddress2 aPort2) : _ = aConnects
            print $ "1| Node adress: " ++ showHostAddress aHostAddress
            void . forkIO $ runClient (showHostAddress aHostAddress) (fromEnum aPort) "/" $ \aConnect -> do
                aMyNodeId <- connectWithNN "1| " PoA aConnect
                print "1| CN is connected to NN."
                checkVersion aConnect
                putMVar aIdOfFirsClientVar aMyNodeId

                void $ takeMVar aSecondNodeIsStartedVar
                print "1| Sending of test broadcast msg..."
                sendMsg aConnect $ MsgBroadcast (IdFrom aMyNodeId) All testMsg
                aNodeId <- receivingOfBroadcast "1| " aConnect
                unless (aNodeId == aMyNodeId) $ error $ "1| The node ID en broadcast msg is broaken."

                aMsg <- receiveMsg aConnect "1| Receiving msg from second node..." "1| Received msg from second node."
                MsgMsgTo _ _ aValue <- pure $ case decode aMsg of
                    Just aMsgTo@(MsgMsgTo _ _ _) -> aMsgTo

                    _                            -> error $ "1| FAIL. The received msg not a correct!"
                unless (aValue == testMsg) $ error "1| The broadcast msg is broaken."

                putMVar testsOk True
                pure ()

            void $ takeMVar aIdOfFirsClientVar
            print "2| Connecting of CN to NN..."
            print $ "2| Node adress: " ++ showHostAddress aHostAddress2
            void . forkIO $ runClient (showHostAddress aHostAddress2) (fromEnum aPort2) "/" $ \aConnect -> do
                aMyId <- connectWithNN "2| " PoA aConnect
                print "2| CN is connected to NN."
                checkVersion aConnect
                putMVar aSecondNodeIsStartedVar True

                aNodeId <- receivingOfBroadcast "2| " aConnect

                print $ "2| Sending msg to firs node..."
                sendMsg aConnect $ MsgMsgTo (IdFrom aMyId) (IdTo aNodeId) testMsg

            aWait
            print ""
            print "-------------"
            print "   -------"
            print "   Pending"
            print "   -------"
            print "-------------"
            print ""
            void . forkIO $ runClient (showHostAddress aHostAddress) (fromEnum aPort) "/" $ \aConnect -> do
                aTransaction : _ <- genNTx 10
                void $ connectWithNN "   " PoA aConnect
                void $ do
                    aTransactions <- checkOfPending aConnect
                    unless (null aTransactions) $ error "  FAIL. Then pending not empty!"

                print "   Sending transaction in the pending."
                void $ do
                    sendMsg aConnect $ MsgTransaction aTransaction
                    aTransactions <- checkOfPending aConnect
                    when (null aTransactions) $ error "   FAIL. Then pending is empty!"

                print "   Checking of pending clearing."
                void $ do

                    sendMsg aConnect $ MsgMicroblock $ genMicroBlock aTransaction
                    aTransactions <- checkOfPending aConnect
                    unless (null aTransactions) $ error "   FAIL. Then pending not empty!"
                    putMVar testsOk True
            aWait
            print ""
            print "---------------------------------------"
            print "   ---------------------------------"
            print "   Resending (MicroBlocs, KeyBlocks)"
            print "   ---------------------------------"
            print "---------------------------------------"
            print ""
            print $ "1| Node adress: " ++ showHostAddress aHostAddress
            aNode1Start <- newEmptyMVar
            void . forkIO $ runClient (showHostAddress aHostAddress) (fromEnum aPort) "/" $ \aConnect -> do
                void $ connectWithNN "1| " All aConnect
                putMVar aNode1Start True
                aMsg1 <- receiveMsg aConnect
                                    ("1| " ++ "Receiving of msg Microblock...")
                                    ("1| " ++ "Received msg Microblock.")
                void $ pure $ case decode aMsg1 of
                    Just (MsgMicroblock _) -> 0 :: Int
                    _                      -> error $ "1| FAIL. The received msg not a correct!"
                aMsg2 <- receiveMsg aConnect
                                    ("1| " ++ "Receiving of msg KeyBlock...")
                                    ("1| " ++ "Received msg KeyBlock.")
                void $ pure $ case decode aMsg2 of
                    Just (MsgKeyBlock _) -> 0 :: Int
                    _                    -> error $ "1| FAIL. The received msg not a correct!"
                putMVar testsOk True

            _ <- takeMVar aNode1Start
            print $ "2| Node adress: " ++ showHostAddress aHostAddress2
            void . forkIO $ runClient (showHostAddress aHostAddress2) (fromEnum aPort2) "/" $ \aConnect -> do
                void $ connectWithNN "2| " PoA aConnect
                aTransaction : _ <- genNTx 10
                print "2| Sending of Microblock"
                sendMsg aConnect $ MsgMicroblock $ genMicroBlock aTransaction
                print "2| Sending of KeyBlock"
                sendMsg aConnect $ MsgKeyBlock testMsg

            aWait
            print ""
            print "-----------------"
            print "   -----------"
            print "   Testing Ok!"
            print "   -----------"
            print "-----------------"
            print ""

        "b" : ip : _ -> runClient ip 1554 "/" $ socketActor
            (\aConnect -> forever $ do
                threadDelay 1000
                WS.sendBinaryData
                    aConnect
                    ("{\"tag\": \"Request\", \"type\": \"Broadcast\", \"recipientType\" : \"PoA\", \"msg\" : { \"str\": \"000000000\"}}" :: T.Text
                    )
            )
        "t" : ip : _ -> do
            aTransactions <- genNTx 1000
            print "Transactions generated"
            runClient ip 1554 "/" $ socketActor $ \aConnect -> forM_ (cycle aTransactions) $ \aTrans -> do
                threadDelay 1000
                WS.sendBinaryData aConnect $ encode $ MsgTransaction aTrans
        "l" : ip : aFile : _ -> do
            aTrafic <- readFile aFile
            runClient ip 1554 "/" $ socketActor $ \aConnect -> forM_ (lines aTrafic) $ \aMsg -> do
                threadDelay 50000
                WS.sendTextData aConnect $ aMsg --B8.pack aMsg
        _ -> pure ()


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
