{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Node.NetLvl.Server (
    netLvlServer,
    msgReceiver,
    msgSender,
    sendActionToCentralActor
  )  where

import qualified Control.Concurrent                    as C
-- import qualified Control.Concurrent.Chan               as C
import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad                         (forever, unless, void,
                                                        when)
import           Data.Aeson                            as A
--import qualified Data.ByteString.Char8                 as B8
import qualified Data.Text                             as T
import qualified Network.WebSockets                    as WS
import           Node.Data.GlobalLoging
import           Node.DataActor
import           Node.NetLvl.Messages
import           Node.Node.Types
import           Pending
import           Service.InfoMsg                       as I
import           Service.Network.Base
import           Service.Network.WebSockets.Server
-- import           Service.Types
import           Service.System.Version
import           System.Random.Shuffle

import           Control.Concurrent.Async
import           Data.Maybe                            ()
import           Node.Data.Key
import           Service.Sync.SyncJson


netLvlServer (MyNodeId aMyNodeId) aReceivePort ch aInfoChan aFileServerChan inChanPending aInChan = do
    writeLog aInfoChan [ServePoATag, InitTag] Info $
        "Init. NetLvlServer: a port is " ++ show aReceivePort
    runServer aReceivePort ("server of SN: " ++ show aMyNodeId) $ \aIp aPending -> do
        aConnect <- WS.acceptRequest aPending
        writeLog aInfoChan [ServePoATag] Info $ "New connect: " ++ showHostAddress aIp
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
                        (msgReceiver ch aInfoChan aFileServerChan aNodeType (IdFrom aNodeId) aConnect inChanPending aInChan)

            Right (ActionConnect aNodeType Nothing) -> do
                aNodeId <- generateClientId []
                WS.sendTextData aConnect $ A.encode $ ResponseNodeId aNodeId
                (aInpChan, aOutChan) <- newChan 64
                sendActionToCentralActor ch $ NewConnect aNodeId aNodeType aInpChan Nothing

                void $ race
                    (msgSender ch aNodeId aConnect aOutChan)
                    (msgReceiver ch aInfoChan aFileServerChan aNodeType (IdFrom aNodeId) aConnect inChanPending aInChan)

            Right _ -> do
                writeLog aInfoChan [ServePoATag] Warning $ "Broken message from PP " ++ show aMsg
                WS.sendTextData aConnect $ T.pack ("{\"tag\":\"Response\",\"type\":\"ErrorOfConnect\", \"Msg\":" ++ show aMsg ++ ", \"comment\" : \"not a connect msg\"}")

            Left a -> do
                writeLog aInfoChan [ServePoATag] Warning $ "Broken message from PP " ++ show aMsg ++ " " ++ a ++ " ip: " ++ showHostAddress aIp
                WS.sendTextData aConnect $ T.pack ("{\"tag\":\"Response\",\"type\":\"ErrorOfConnect\", \"reason\":\"" ++ a ++ "\", \"Msg\":" ++ show aMsg ++"}")


msgSender
    ::  ToJSON a1
    =>  InChan MsgToCentralActor
    ->  NodeId
    ->  WS.Connection
    ->  OutChan a1
    ->  IO a2
msgSender ch aId aConnect aNewChan = forever (WS.sendTextData aConnect . A.encode =<< readChan aNewChan)
    `finally` writeChan ch (NodeIsDisconnected aId)


msgReceiver ch aInfoChan aFileServerChan aNodeType aId aConnect aPendingChan aInChan = forever $ do
    aMsg <- WS.receiveData aConnect
    let aLog aMsg  = writeLog aInfoChan [ServePoATag] Info aMsg
        aSend aMsg = WS.sendTextData aConnect $ A.encode aMsg
    aLog $ "Raw msg: " ++ show aMsg ++ "\n"
    case A.eitherDecodeStrict aMsg of
        Right a -> case a of
            -- REVIEW: Check fair distribution of transactions between nodes
            RequestTransaction aNum -> void $ C.forkIO $ do
                aTmpChan <- C.newChan
                writeInChan aPendingChan $ GetTransaction aNum aTmpChan
                aTransactions <- C.readChan aTmpChan
                aLog    "Sending of transactions to client node."
                aSend $ ResponseTransactions aTransactions

            RequestPotentialConnects _ -> do
                aShuffledRecords <- shuffleM =<< getRecords aFileServerChan
                let aConnects = take 5 aShuffledRecords
                aLog  $ "Sending of connections: " ++ show aConnects
                aSend $ ResponsePotentialConnects aConnects

            RequestPoWList -> do
                aLog $ "PoWListRequest the msg from " ++ show aId
                sendActionToCentralActor ch $ RequestListOfPoW aId

            RequestVersion -> do
                aLog  $ "Version request from client node."
                aSend $ ResponseVersion $(version)

            RequestPending (Just aTransaction) -> do
                aTmpChan <- C.newChan
                writeInChan aPendingChan $ IsInPending aTransaction aTmpChan
                aTransactions <- C.readChan aTmpChan
                aLog  $ "Pending request from client node."
                aSend $ ResponseTransactionIsInPending aTransactions

            RequestPending Nothing -> do
                aTmpChan <- C.newChan
                writeInChan aPendingChan $ GetPending aTmpChan
                aTransactions <- C.readChan aTmpChan
                aSend $ ResponseTransactions aTransactions


            RequestActualConnects -> do
                aMVar <- newEmptyMVar
                sendActionToCentralActor ch $ RequestActualConnectList aMVar
                aVar <- takeMVar aMVar
                aSend $ ResponseActualConnects aVar
            --
            aMessage -> do
                writeLog aInfoChan [ServePoATag] Info $ "Received msg " ++ show aMessage
                sendMsgToCentralActor ch aNodeType aMessage
                when (isBlock aMessage) $ writeInChan aInChan aMsg

        Left a -> do
            case A.eitherDecodeStrict aMsg of
                Right (a :: SyncMessage) -> do
                    let IdFrom aNodeId = aId
                    writeInChan ch (SyncToNode a aNodeId)
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
