{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PoA.PoAServer (
    servePoA,
    msgReceiver,
    msgSender,
    sendActionToCentralActor
  )  where

import              Control.Monad (forM_, void, when, forever, unless)
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


servePoA
    :: MyNodeId
    -> PortNumber
    -> InChan MsgToCentralActor
    -> InChan InfoMsg
    -> InChan FileActorRequest
    -> InChan Microblock
    -> InChan PendingAction
    -> IO ()
servePoA (MyNodeId aMyNodeId) aRecivePort ch aInfoChan aFileServerChan aMicroblockChan inChanPending = do
    writeLog aInfoChan [ServePoATag, InitTag] Info $
        "Init. servePoA: a port is " ++ show aRecivePort
    runServer aRecivePort $ \aIp aPending -> do
        aConnect <- WS.acceptRequest aPending
        WS.forkPingThread aConnect 30
        aMsg <- WS.receiveData aConnect
        case A.eitherDecodeStrict aMsg of
            Right (ActionConnect aNodeType (Just aNodeId))
                | NodeId aMyNodeId /= aNodeId -> do
                    (aInpChan, aOutChan) <- newChan 64

                    when (aNodeType == NN) .
                        WS.sendTextData aConnect . A.encode $ ActionConnect NN (Just (NodeId aMyNodeId))

                    sendActionToCentralActor ch $ NewConnect aNodeId aNodeType aInpChan Nothing

                    void $ race
                        (msgSender ch aNodeId aConnect aOutChan)
                        (msgReceiver ch aInfoChan aFileServerChan aNodeType (IdFrom aNodeId) aConnect inChanPending)

            Right (ActionConnect aNodeType Nothing) -> do
                aNodeId <- generateClientId []
                WS.sendTextData aConnect $ A.encode $ ResponseNodeId aNodeId
                (aInpChan, aOutChan) <- newChan 64
                sendActionToCentralActor ch $ NewConnect aNodeId aNodeType aInpChan Nothing

                void $ race
                    (msgSender ch aNodeId aConnect aOutChan)
                    (msgReceiver ch aInfoChan aFileServerChan aNodeType (IdFrom aNodeId) aConnect inChanPending)

            Right _ -> do
                writeLog aInfoChan [ServePoATag] Warning $ "Broken message from PP " ++ show aMsg
                WS.sendTextData aConnect $ T.pack ("{\"tag\":\"Response\",\"type\":\"ErrorOfConnect\", \"Msg\":" ++ show aMsg ++ ", \"comment\" : \"not a connect msg\"}")

            Left a -> do
                writeLog aInfoChan [ServePoATag] Warning $ "Broken message from PP " ++ show aMsg ++ " " ++ a ++ " ip: " ++ showHostAddress aIp
                WS.sendTextData aConnect $ T.pack ("{\"tag\":\"Response\",\"type\":\"ErrorOfConnect\", \"reason\":\"" ++ a ++ "\", \"Msg\":" ++ show aMsg ++"}")

msgSender ch aId aConnect aNewChan = forever (WS.sendTextData aConnect . A.encode =<< readChan aNewChan)
    `finally` writeChan ch (NodeIsDisconnected aId)

msgReceiver ch aInfoChan aFileServerChan aNodeType aId aConnect aPendingChan = forever $ do
    aMsg <- WS.receiveData aConnect
    writeLog aInfoChan [ServePoATag] Info $ "Raw msg: " ++ show aMsg
    case A.eitherDecodeStrict aMsg of
        Right a -> case a of
            -- REVIEW: Check fair distribution of transactions between nodes
            RequestTransaction aNum -> void $ C.forkIO $ do
                aTmpChan <- C.newChan
                writeInChan aPendingChan $ GetTransaction aNum aTmpChan
                aTransactions <- C.readChan aTmpChan
                writeLog aInfoChan [ServePoATag] Info "sendTransactions to poa"
                WS.sendTextData aConnect $ A.encode $ ResponseTransactions aTransactions

            RequestPotentialConnects _ -> do
                aShuffledRecords <- shuffleM =<< getRecords aFileServerChan
                let aConnects = take 5 aShuffledRecords
                writeLog aInfoChan [ServePoATag] Info $ "Send connections " ++ show aConnects
                WS.sendTextData aConnect $ A.encode $ ResponsePotentialConnects aConnects

            RequestPoWList -> do
                    writeLog aInfoChan [ServePoATag] Info $
                        "PoWListRequest the msg from " ++ show aId
                    sendActionToCentralActor ch $ RequestListOfPoW aId

            RequestPending (Just aTransaction) -> do
                aTmpChan <- C.newChan
                writeInChan aPendingChan $ IsInPending aTransaction aTmpChan
                aTransactions <- C.readChan aTmpChan
                WS.sendTextData aConnect $ A.encode $ ResponseTransactionIsInPending aTransactions

            RequestPending Nothing -> do
                aTmpChan <- C.newChan
                writeInChan aPendingChan $ GetPending aTmpChan
                aTransactions <- C.readChan aTmpChan
                WS.sendTextData aConnect $ A.encode $ ResponseTransactions aTransactions


            RequestActualConnects -> do
                aMVar <- newEmptyMVar
                sendActionToCentralActor ch $ RequestActualConnectList aMVar
                WS.sendTextData aConnect . A.encode . ResponseActualConnects =<< takeMVar aMVar
            --
            aMsg -> do
                writeLog aInfoChan [ServePoATag] Info $ "Received msg " ++ show aMsg
                sendMsgToCentralActor ch aNodeType aMsg

            _ -> return ()
        Left a -> do
            writeLog aInfoChan [ServePoATag] Warning $ "Broken message from PP " ++ show aMsg ++ " " ++ a
            WS.sendTextData aConnect $ T.pack ("{\"tag\":\"Response\",\"type\":\"Error\", \"reason\":\"" ++ a ++ "\", \"Msg\":" ++ show aMsg ++"}")


writeInChan :: InChan t -> t -> IO ()
writeInChan aChan aMsg = do
    aOk <- tryWriteChan aChan aMsg
    C.threadDelay 10000
    unless aOk $ writeInChan aChan aMsg


sendMsgToCentralActor :: InChan MsgToCentralActor -> NodeType -> NetMessage -> IO ()
sendMsgToCentralActor aChan aNodeType aMsg = writeInChan aChan (MsgFromNode aNodeType aMsg)


sendActionToCentralActor :: InChan MsgToCentralActor -> MsgFromNode -> IO ()
sendActionToCentralActor aChan aMsg = writeInChan aChan (ActionFromNode aMsg)
