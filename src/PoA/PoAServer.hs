{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PoA.PoAServer (
        servePoA
  )  where

import              Control.Monad (forM_, void, forever, unless)
import qualified    Network.WebSockets                  as WS
import              Control.Concurrent.MVar
import              Service.Network.Base
import              Service.Network.WebSockets.Server
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
import qualified    Control.Concurrent as C
import              Node.FileDB.FileServer
import              PoA.Pending

import              Control.Concurrent.Async
import              Node.Data.Key
import              Data.Maybe()


servePoA ::
       PortNumber
    -> InChan MsgToCentralActor
    -> OutChan (Transaction, MVar Bool)
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

        WS.sendTextData aConnect $ A.encode RequestNodeIdToPP
        aMsg <- WS.receiveData aConnect
        case A.eitherDecodeStrict aMsg of
            Right (NNConnection _aPortNumber _aPublicPoint _aNodeId) -> return ()
            Right (CNConnection aNodeType (Just aNodeId)) -> do
                (aInpChan, aOutChan) <- newChan 64
                sendMsgToCentralActor ch $ NewConnect aNodeId aNodeType aInpChan Nothing

                void $ race
                    (aSender aNodeId aConnect aOutChan)
                    (aReceiver (IdFrom aNodeId) aConnect inChanPending)
            Right (CNConnection aNodeType Nothing) -> do
                aNodeId <- generateClientId []
                WS.sendTextData aConnect $ A.encode $ ResponseClientId aNodeId
                (aInpChan, aOutChan) <- newChan 64
                sendMsgToCentralActor ch $ NewConnect aNodeId aNodeType aInpChan Nothing

                void $ race
                    (aSender aNodeId aConnect aOutChan)
                    (aReceiver (IdFrom aNodeId) aConnect inChanPending)
            Right (ResponseNodeIdToNN aNodeId aNodeType) -> do
                (aInpChan, aOutChan) <- newChan 64
                sendMsgToCentralActor ch $ NewConnect aNodeId aNodeType aInpChan Nothing

                void $ race
                    (aSender aNodeId aConnect aOutChan)
                    (aReceiver (IdFrom aNodeId) aConnect inChanPending)

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
                            BroadcastRequest aBroadcastMsg aId aRecipientType
                RequestConnects _ -> do
                    aShuffledRecords <- shuffleM =<< getRecords aFileServerChan
                    let aConnects = take 5 aShuffledRecords
                    writeLog aInfoChan [ServePoATag] Info $ "Send connections " ++ show aConnects
                    WS.sendTextData aConnect $ A.encode $ ResponseConnects
                        ((\(Connect this_ip _) -> Connect this_ip 1554) <$> aConnects)

                RequestPoWList -> do
                        writeLog aInfoChan [ServePoATag] Info $
                            "PoWListRequest the msg from " ++ show aId
                        sendMsgToCentralActor ch $ PoWListRequest aId

                MsgMsgToNN aDestination aMsgToNN -> do
                        writeLog aInfoChan [ServePoATag] Info $
                            "Resending the msg from " ++ show aId ++ " the msg is " ++ show aMsgToNN
                        sendMsgToCentralActor ch $ MsgResending aId (IdTo aDestination) aMsgToNN

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
                    aMVar <- newEmptyMVar
                    writeInChan ch $ NewTransaction aTransaction aMVar
                    WS.sendTextData aConnect . A.encode . ResponseTransactionValid =<< takeMVar aMVar

                RequestActualConnectList -> do
                    aMVar <- newEmptyMVar
                    sendMsgToCentralActor ch $ ActualConnectListRequest aMVar
                    WS.sendTextData aConnect . A.encode . ActualConnectList =<< takeMVar aMVar

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
