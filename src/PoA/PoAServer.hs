{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PoA.PoAServer (
        servePoA
    ,   serverPoABootNode
  )  where



import              Control.Monad (forM_, void, forever, unless, when)
import qualified    Network.WebSockets                  as WS
import              Service.Network.Base
import              Service.Network.WebSockets.Server
import              Service.Network.WebSockets.Client
import qualified    Control.Concurrent.Chan as C
import              Control.Concurrent.Chan.Unagi.Bounded
import              Node.Node.Types
import              Service.InfoMsg as I
import qualified    Data.Text as T
import              Service.Types
import              System.Random.Shuffle
import              Data.Aeson as A
import              Control.Exception
import              Node.Data.GlobalLoging
import              PoA.Types
import              Control.Concurrent.MVar
import qualified    Control.Concurrent as C
import              Node.FileDB.FileServer
import              PoA.Pending

import              Control.Concurrent.Async
import              Node.Data.Key
import              Data.Maybe()


data ConnectTesterActor = AddConnectToList Connect | TestExistedConnect Connect

serverPoABootNode :: PortNumber -> InChan InfoMsg -> InChan FileActorRequest -> IO ()
serverPoABootNode aRecivePort aInfoChan aFileServerChan = do
    writeLog aInfoChan [ServerBootNodeTag, InitTag] Info $
        "Init. ServerPoABootNode: a port is " ++ show aRecivePort

    (aInChan, aOutChan) <- newChan 64
    void $ C.forkIO $ forever $ readChan aOutChan >>= \case
        AddConnectToList aConn@(Connect aHostAdress aPort) -> void $ C.forkIO $ do
            C.threadDelay 3000000
            runClient (showHostAddress aHostAdress) (fromEnum aPort) "/" $
                \_ -> void $ tryWriteChan aFileServerChan $ AddToFile [aConn]

        TestExistedConnect aConn@(Connect aHostAdress aPort) -> void $ C.forkIO $ do
            aConnects <- getRecords aFileServerChan
            when (aConn`elem`aConnects) $ do
                aOk <- try $ runClient (showHostAddress aHostAdress) (fromEnum aPort) "/" $ \_ -> return ()
                case aOk of
                    Left (_ :: SomeException) ->
                        void $ tryWriteChan aFileServerChan $ DeleteFromFile aConn
                    _ -> return ()

    runServer aRecivePort $ \aHostAdress aPending -> do
        aConnect <- WS.acceptRequest aPending
        writeLog aInfoChan [ServerBootNodeTag] Info "ServerPoABootNode.Connect accepted."
        aMsg <- WS.receiveData aConnect
        case A.eitherDecodeStrict aMsg of
            Right a -> case a of
                RequestConnects aFull
                    | aFull -> do
                        writeLog aInfoChan [ServerBootNodeTag] Info "Accepted request full list of connections."
                        aConnects <- getRecords aFileServerChan
                        WS.sendTextData aConnect $ A.encode $ ResponseConnects aConnects

                    | otherwise -> do
                        writeLog aInfoChan [ServerBootNodeTag] Info "Accepted request of connections."
                        aShuffledRecords <- shuffleM =<< getRecords aFileServerChan
                        let aConnects = take 5 aShuffledRecords
                        WS.sendTextData aConnect $ A.encode $ ResponseConnects aConnects

                ActionAddToListOfConnects aPort ->
                    void $ tryWriteChan aInChan $ AddConnectToList (Connect aHostAdress (toEnum aPort))

                ActionNodeStillAliveTest aPort aIp ->
                    void $ tryWriteChan aInChan $ TestExistedConnect (Connect aIp aPort)

                _  -> writeLog aInfoChan [ServerBootNodeTag] Warning $
                    "Broken message from PP " ++ show aMsg
            Left a -> do
                WS.sendTextData aConnect $ T.pack ("{\"tag\":\"Response\",\"type\":\"Error\", \"reason\":\"" ++ a ++ "\", \"Msg\":" ++ show aMsg ++"}")
                writeLog aInfoChan [ServerBootNodeTag] Warning $
                    "Broken message from PP " ++ show aMsg ++ " " ++ a


getRecords :: InChan FileActorRequest -> IO [Connect]
getRecords aChan = do
    aTmpRef <- newEmptyMVar
    writeChan aChan $ ReadRecordsFromFile aTmpRef
    takeMVar aTmpRef

servePoA ::
       PortNumber
    -> InChan MsgToCentralActor
    -> OutChan Transaction
    -> InChan InfoMsg
    -> InChan FileActorRequest
    -> InChan Microblock
    -> IO ()
servePoA aRecivePort ch aRecvChan aInfoChan aFileServerChan aMicroblockChan = do
    writeLog aInfoChan [ServePoATag, InitTag] Info $
        "Init. servePoA: a port is " ++ show aRecivePort
    aPendingChan@(inChanPending, _) <- newChan 120
    void $ C.forkIO $ pendingActor aPendingChan aMicroblockChan aRecvChan aInfoChan
    runServer aRecivePort $ \_ aPending -> do
        aConnect <- WS.acceptRequest aPending
        WS.forkPingThread aConnect 30

        aMsg <- WS.receiveData aConnect
        case A.eitherDecodeStrict aMsg of
            Right (NNConnection _aPortNumber _aPublicPoint _aNodeId) -> return ()
            Right (CNConnection aNodeType (Just aNodeId)) -> do
                (aInpChan, aOutChan) <- newChan 64
                sendMsgToCentralActor ch $ NewConnect aNodeId aNodeType aInpChan

                void $ race
                    (aSender aNodeId aConnect aOutChan)
                    (aReceiver aNodeId aConnect inChanPending)
            Right (CNConnection aNodeType Nothing) -> do
                aNodeId <- generateClientId []
                WS.sendTextData aConnect $ A.encode $ ResponseClientId aNodeId
                (aInpChan, aOutChan) <- newChan 64
                sendMsgToCentralActor ch $ NewConnect aNodeId aNodeType aInpChan

                void $ race
                    (aSender aNodeId aConnect aOutChan)
                    (aReceiver aNodeId aConnect inChanPending)

            Right _ -> do
                writeLog aInfoChan [ServePoATag] Warning $ "Broken message from PP " ++ show aMsg
                WS.sendTextData aConnect $ T.pack ("{\"tag\":\"Response\",\"type\":\"ErrorOfConnect\", \"Msg\":" ++ show aMsg ++ ", \"comment\" : \"not a connect msg\"}")

            Left a -> do
                writeLog aInfoChan [ServePoATag] Warning $ "Broken message from PP " ++ show aMsg ++ " " ++ a
                WS.sendTextData aConnect $ T.pack ("{\"tag\":\"Response\",\"type\":\"ErrorOfConnect\", \"reason\":\"" ++ a ++ "\", \"Msg\":" ++ show aMsg ++"}")

  where
    aSender aId aConnect aNewChan = forever (WS.sendTextData aConnect . A.encode =<< readChan aNewChan)
        `finally` writeChan ch (NodeIsDisconnected aId)

    aReceiver aId aConnect aPendingChan = forever $ do
        aMsg <- WS.receiveData aConnect
        writeLog aInfoChan [ServePoATag] Info $ "Raw msg: " ++ show aMsg
        case A.eitherDecodeStrict aMsg of
            Right a -> case a of
                -- REVIEW: Check fair distribution of transactions between nodes
                RequestTransaction aNum -> void $ C.forkIO $ do
                    aTmpChan <- C.newChan
                    writeInChan aPendingChan $ GetTransaction aNum aTmpChan
                    aTransactions <- C.readChan aTmpChan
                    unless (null aTransactions) $
                        forM_ (take aNum $ cycle aTransactions) $ \aTransaction  -> do
                            writeLog aInfoChan [ServePoATag] Info $  "sendTransaction to poa " ++ show aTransaction
                            WS.sendTextData aConnect $ A.encode $ ResponseTransaction aTransaction
                MsgMicroblock aMicroblock -> do
                        writeLog aInfoChan [ServePoATag] Info $ "Recived MBlock: " ++ show aMicroblock
                        sendMsgToCentralActor ch $ AcceptedMicroblock aMicroblock aId

                RequestBroadcast aRecipientType aBroadcastMsg -> do
                        writeLog aInfoChan [ServePoATag] Info $ "Broadcast request " ++ show aMsg
                        sendMsgToCentralActor ch $
                            BroadcastRequest aBroadcastMsg (IdFrom aId) aRecipientType
                RequestConnects _ -> do
                    aShuffledRecords <- shuffleM =<< getRecords aFileServerChan
                    let aConnects = take 5 aShuffledRecords
                    writeLog aInfoChan [ServePoATag] Info $ "Send connections " ++ show aConnects
                    WS.sendTextData aConnect $ A.encode $ ResponseConnects
                        ((\(Connect this_ip _) -> Connect this_ip 1554) <$> aConnects)

                RequestPoWList -> do
                        writeLog aInfoChan [ServePoATag] Info $
                            "PoWListRequest the msg from " ++ show aId
                        sendMsgToCentralActor ch $ PoWListRequest (IdFrom aId)

                MsgMsgToNN aDestination aMsgToNN -> do
                        writeLog aInfoChan [ServePoATag] Info $
                            "Resending the msg from " ++ show aId ++ " the msg is " ++ show aMsgToNN
                        sendMsgToCentralActor ch $ MsgResending (IdFrom aId) (IdTo aDestination) aMsgToNN

                IsInPendingRequest aTransaction -> do
                    aTmpChan <- C.newChan
                    writeInChan aPendingChan $ IsInPending aTransaction aTmpChan
                    aTransactions <- C.readChan aTmpChan
                    WS.sendTextData aConnect $ A.encode $ ResponseIsInPending aTransactions

                GetPendingRequest -> do
                    aTmpChan <- C.newChan
                    writeInChan aPendingChan $ GetPending aTmpChan
                    aTransactions <- C.readChan aTmpChan
                    WS.sendTextData aConnect $ A.encode $ ResponsePendingTransactions aTransactions

                AddTransactionRequest aTransaction -> do
                    writeInChan ch $ NewTransaction aTransaction
                    WS.sendTextData aConnect $ A.encode $ ResponseTransactionValid True

                _ -> return ()
            Left a -> do
                writeLog aInfoChan [ServePoATag] Warning $ "Broken message from PP " ++ show aMsg ++ " " ++ a
                WS.sendTextData aConnect $ T.pack ("{\"tag\":\"Response\",\"type\":\"Error\", \"reason\":\"" ++ a ++ "\", \"Msg\":" ++ show aMsg ++"}")


writeInChan :: InChan t -> t -> IO ()
writeInChan aChan aMsg = do
    aOk <- tryWriteChan aChan aMsg
    C.threadDelay 10000
    unless aOk $ writeInChan aChan aMsg

sendMsgToCentralActor :: InChan MsgToCentralActor -> MsgFromNode -> IO ()
sendMsgToCentralActor aChan aMsg = writeInChan aChan (MsgFromNode aMsg)
