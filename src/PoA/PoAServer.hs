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
import              Data.Either


data ConnectTesterActor = AddConnectToList Connect | TestExistedConnect Connect

serverPoABootNode :: PortNumber -> InChan InfoMsg -> InChan FileActorRequest -> IO ()
serverPoABootNode aRecivePort aInfoChan aFileServerChan = do
    writeLog aInfoChan [ServerBootNodeTag, InitTag] Info $
        "Init. ServerPoABootNode: a port is " ++ show aRecivePort

    (aInChan, aOutChan) <- newChan 64
    void $ C.forkIO $ forever $ readChan aOutChan >>= \case
        AddConnectToList aConn@(Connect aHostAdress aPort) -> void $ C.forkIO $
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

{-
writeChan (aData^.fileServerChan) $
        FileActorRequestNetLvl $ UpdateFile (aData^.myNodeId)
        (NodeInfoListNetLvl [(aNodeId, Connect (aNode^.nodeHost) (aNode^.nodePort))])

-}

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
    -> OutChan Microblock
    -> IO ()
servePoA aRecivePort ch aRecvChan aInfoChan aFileServerChan aMicroblockChan = do
    writeLog aInfoChan [ServePoATag, InitTag] Info $
        "Init. servePoA: a port is " ++ show aRecivePort
    aPendingChan@(inChanPending, _) <- newChan 120
    void $ C.forkIO $ pendingActor aPendingChan aMicroblockChan aRecvChan aInfoChan
    runServer aRecivePort $ \_ aPending -> do
        aConnect <- WS.acceptRequest aPending
        WS.forkPingThread aConnect 30

        WS.sendTextData aConnect $ A.encode RequestNodeIdToPP
        aId <- newEmptyMVar
        (aInpChan, aOutChan) <- newChan 64
        -- writeChan ch $ connecting to PoA, the PoA have id.
        void $ race
            (aSender aId aConnect aOutChan)
            (aReceiver aId aConnect aInpChan inChanPending)
  where
    aSender aId aConnect aNewChan = forever (do
        aMsg <- readChan aNewChan
        WS.sendTextData aConnect $ A.encode aMsg) `finally` (do
            aIsEmpty <- isEmptyMVar aId
            unless aIsEmpty $ do
                aDeadId <- readMVar aId
                writeChan ch $ NodeIsDisconnected aDeadId)

    aReceiver aId aConnect aNewChan aPendingChan = forever $ do
        aMsg <- WS.receiveData aConnect
        writeLog aInfoChan [ServePoATag] Info $ "Raw msg: " ++ show aMsg
        aOk <- isEmptyMVar aId
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
                MsgMicroblock aMicroblock
                    | not aOk -> do
                        aSenderId <- readMVar aId
                        writeLog aInfoChan [ServePoATag] Info $ "Recived MBlock: " ++ show aMicroblock
                        sendMsgToCentralActor ch $ AcceptedMicroblock aMicroblock aSenderId
                    | otherwise -> do
                        writeLog aInfoChan [ServePoATag] Warning $ "Broadcast request  without PPId " ++ show aMsg
                        WS.sendTextData aConnect $ A.encode RequestNodeIdToPP

                RequestBroadcast aRecipientType aBroadcastMsg
                    | not aOk -> do
                        aSenderId <- readMVar aId
                        writeLog aInfoChan [ServePoATag] Info $ "Broadcast request " ++ show aMsg
                        sendMsgToCentralActor ch $
                            BroadcastRequest aBroadcastMsg (IdFrom aSenderId) aRecipientType
                    | otherwise -> do
                        writeLog aInfoChan [ServePoATag] Warning $ "Broadcast request  without PPId " ++ show aMsg
                        WS.sendTextData aConnect $ A.encode RequestNodeIdToPP
                RequestConnects _ -> do
                    aShuffledRecords <- shuffleM =<< getRecords aFileServerChan
                    let aConnects = take 5 aShuffledRecords
                    writeLog aInfoChan [ServePoATag] Info $ "Send connections " ++ show aConnects
                    WS.sendTextData aConnect $ A.encode $ ResponseConnects
                        ((\(Connect this_ip _) -> Connect this_ip 1554) <$> aConnects)

                RequestPoWList
                    | not aOk -> do
                        aSenderId <- readMVar aId
                        writeLog aInfoChan [ServePoATag] Info $
                            "PoWListRequest the msg from " ++ show aSenderId
                        sendMsgToCentralActor ch $ PoWListRequest (IdFrom aSenderId)

                    | otherwise -> do
                        writeLog aInfoChan [ServePoATag] Warning "Can't send request without PPId "
                        WS.sendTextData aConnect $ A.encode RequestNodeIdToPP


                ResponseNodeIdToNN aPPId aNodeType ->
                    when aOk $ do
                        putMVar aId aPPId
                        writeLog aInfoChan [ServePoATag] Info $
                            "Accept PPId " ++ show aPPId ++ " with type " ++ show aNodeType

                        sendMsgToCentralActor ch $ NewConnect aPPId aNodeType aNewChan

                MsgMsgToNN aDestination aMsgToNN
                    | not aOk       -> do
                        aSenderId <- readMVar aId
                        writeLog aInfoChan [ServePoATag] Info $
                            "Resending the msg from " ++ show aSenderId ++ " the msg is " ++ show aMsgToNN
                        sendMsgToCentralActor ch $ MsgResending (IdFrom aSenderId) (IdTo aDestination) aMsgToNN
                    | otherwise     -> do
                        writeLog aInfoChan [ServePoATag] Warning $ "Can't send request without PPId " ++ show aMsgToNN
                        WS.sendTextData aConnect $ A.encode RequestNodeIdToPP
                --
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
                -- TODO: Include ID if exist.
                writeLog aInfoChan [ServePoATag] Warning $
                    "Broken message from PP " ++ show aMsg ++ " " ++ a
                WS.sendTextData aConnect $ T.pack ("{\"tag\":\"Response\",\"type\":\"Error\", \"reason\":\"" ++ a ++ "\", \"Msg\":" ++ show aMsg ++"}")
                when aOk $ WS.sendTextData aConnect $ A.encode RequestNodeIdToPP

writeInChan :: InChan t -> t -> IO ()
writeInChan aChan aMsg = do
    aOk <- tryWriteChan aChan aMsg
    C.threadDelay 10000
    unless aOk $ writeInChan aChan aMsg

sendMsgToCentralActor :: InChan MsgToCentralActor -> MsgFromNode -> IO ()
sendMsgToCentralActor aChan aMsg = writeInChan aChan (MsgFromNode aMsg)
