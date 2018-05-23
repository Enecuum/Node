{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module PoA.PoAServer (
        servePoA
    ,   serverPoABootNode
  )  where


import              Node.Data.NetPackage
import              Control.Monad (forM_, void, forever, unless, when)
import qualified    Network.WebSockets                  as WS
import              Service.Network.Base
import              Service.Network.WebSockets.Server
import              Control.Concurrent.Chan
import              Node.Node.Types
import              Service.InfoMsg as I
import              Service.Types
import              System.Random.Shuffle
import              Data.String
import qualified    Data.ByteString as B
import              Data.Aeson as A
import              Control.Exception
import              Node.Data.GlobalLoging
import              PoA.Types
import              Control.Concurrent.MVar
import              Control.Concurrent
import              Node.FileDB.FileServer

import              Control.Concurrent.Async
import              Node.Data.Key
import              Data.Maybe()


-- :m PoA.PoAServer
--

--undead f = finally f (undead f)

--
serverPoABootNode :: PortNumber -> Chan InfoMsg -> Chan FileActorRequest -> IO ()
serverPoABootNode aRecivePort aInfoChan aFileServerChan = do
    writeLog aInfoChan [ServerBootNodeTag, InitTag] Info $
        "Init. ServerPoABootNode: a port is " ++ show aRecivePort
    runServer aRecivePort $ \_ aPending -> do
        aConnect <- WS.acceptRequest aPending
        writeLog aInfoChan [ServerBootNodeTag] Info "ServerPoABootNode.Connect accepted."
        WS.forkPingThread aConnect 30
        forever $ do
            aMsg <- WS.receiveData aConnect
            case A.eitherDecodeStrict aMsg of
                Right a -> case a of
                    RequestConnects -> do
                        writeLog aInfoChan [ServerBootNodeTag] Info "Accepted request of connections."
                        aConChan <- newChan
                        writeChan aFileServerChan $ FileActorRequestNetLvl $ ReadRecordsFromNodeListFile aConChan
                        NodeInfoListNetLvl aRecords <- readChan aConChan
                        aShuffledRecords <- shuffleM aRecords
                        let aConnects = snd <$> take 5 aShuffledRecords
                        WS.sendTextData aConnect $ A.encode $ ResponseConnects aConnects
                        writeLog aInfoChan [ServerBootNodeTag] Info $ "Send connections " ++ show aConnects
                    _  -> writeLog aInfoChan [ServerBootNodeTag] Warning $
                        "Brouken message from PP " ++ show aMsg ++ "" ++ (show a)
                Left a ->
                    -- TODO: Вписать ID если такой есть.
                    writeLog aInfoChan [ServerBootNodeTag] Warning $
                        "Brouken message from PP " ++ show aMsg ++ " " ++ a


servePoA ::
       PortNumber
    -> MyNodeId
    -> Chan ManagerMiningMsgBase
    -> Chan Transaction
    -> Chan InfoMsg
    -> Chan FileActorRequest
    -> IO ()
servePoA aRecivePort aNodeId ch aRecvChan aInfoChan aFileServerChan = do
    writeLog aInfoChan [ServePoATag, InitTag] Info $
        "Init. servePoA: a port is " ++ show aRecivePort
    runServer aRecivePort $ \_ aPending -> do
        aConnect <- WS.acceptRequest aPending
        WS.forkPingThread aConnect 30

        WS.sendTextData aConnect $ A.encode RequestNodeIdToPP
        aId <- newEmptyMVar
        aNewChan  <- newChan
        -- writeChan ch $ connecting to PoA, the PoA have id.
        void $ race
            (aSender aId aConnect aNewChan)
            (aReceiver aId aConnect aNewChan)
  where
    aSender aId aConnect aNewChan = forever (do
        aMsg <- readChan aNewChan
        WS.sendTextData aConnect $ A.encode aMsg) `finally` (do
            aIsEmpty <- isEmptyMVar aId
            unless aIsEmpty $ do
                aDeadId <- readMVar aId
                writeChan ch $ ppNodeIsDisconected aDeadId)

    aReceiver aId aConnect aNewChan = forever $ do
        aMsg <- WS.receiveData aConnect
        aOk <- isEmptyMVar aId
        case A.eitherDecodeStrict aMsg of
            Right a -> case a of
                -- REVIEW: Check fair distribution of transactions between nodes
                RequestTransaction aNum -> void $ forkIO $ forM_ [1..aNum] $ \_  -> do
                        aTransaction <- readChan aRecvChan
                        writeChan aInfoChan $ Metric $ add
                            ("net.node." ++ show (toInteger aNodeId) ++ ".pending.amount")
                            (-1 :: Integer)
                        writeLog aInfoChan [ServePoATag] Info $  "sendTransaction to poa " ++ show aTransaction
                        WS.sendTextData aConnect
                            (fromString.show $ A.encode $ ResponseTransaction aTransaction :: B.ByteString)
                MsgMicroblock aMicroblock
                    | not aOk -> do
                        aSenderId <- readMVar aId
                        writeLog aInfoChan [ServePoATag] Info $ "Recived MBlock: " ++ show aMicroblock
                        sendMsgToNetLvlFromPP ch $ MicroblockFromPP aMicroblock aSenderId
                    | otherwise -> do
                        writeLog aInfoChan [ServePoATag] Warning $ "Broadcast request  without PPId " ++ show aMsg
                        WS.sendTextData aConnect $ A.encode RequestNodeIdToPP

                RequestBroadcast aRecipientType aBroadcastMsg
                    | not aOk -> do
                        aSenderId <- readMVar aId
                        writeLog aInfoChan [ServePoATag] Info $ "Broadcast request " ++ show aMsg
                        sendMsgToNetLvlFromPP ch $
                            BroadcastRequestFromPP aBroadcastMsg (IdFrom aSenderId) aRecipientType
                    | otherwise -> do
                        writeLog aInfoChan [ServePoATag] Warning $ "Broadcast request  without PPId " ++ show aMsg
                        WS.sendTextData aConnect $ A.encode RequestNodeIdToPP
                RequestConnects -> do
                    aConChan <- newChan
                    writeChan (aFileServerChan) $
                         FileActorRequestNetLvl $ ReadRecordsFromNodeListFile aConChan
                    NodeInfoListNetLvl aRecords <- readChan aConChan
                    aShuffledRecords <- shuffleM aRecords
                    let aConnects = snd <$> take 5 aShuffledRecords
                    writeLog aInfoChan [ServePoATag] Info $ "Send connections " ++ show aConnects
                    WS.sendTextData aConnect $ A.encode $ ResponseConnects aConnects

                RequestPoWList
                    | not aOk -> do
                        aSenderId <- readMVar aId
                        writeLog aInfoChan [ServePoATag] Info $
                            "PoWListRequest the msg from " ++ show aSenderId
                        sendMsgToNetLvlFromPP ch $ PoWListRequest (IdFrom aSenderId)

                    | otherwise -> do
                        writeLog aInfoChan [ServePoATag] Warning "Can't send request without PPId "
                        WS.sendTextData aConnect $ A.encode RequestNodeIdToPP


                ResponseNodeIdToNN aPPId aNodeType ->
                    when aOk $ do
                        putMVar aId aPPId
                        writeLog aInfoChan [ServePoATag] Info $
                            "Accept PPId " ++ show aPPId ++ " with type " ++ show aNodeType

                        sendMsgToNetLvlFromPP ch $ NewConnectWithPP aPPId aNodeType aNewChan

                MsgMsgToNN aDestination aMsgToNN
                    | not aOk       -> do
                        aSenderId <- readMVar aId
                        writeLog aInfoChan [ServePoATag] Info $
                            "Resending the msg from " ++ show aSenderId ++ " the msg is " ++ show aMsgToNN
                        sendMsgToNetLvlFromPP ch $ MsgResendingToPP (IdFrom aSenderId) (IdTo aDestination) aMsgToNN
                    | otherwise     -> do
                        writeLog aInfoChan [ServePoATag] Warning $ "Can't send request without PPId " ++ show aMsgToNN
                        WS.sendTextData aConnect $ A.encode RequestNodeIdToPP

            Left a -> do
                -- TODO: Вписать ID если такой есть.
                writeLog aInfoChan [ServePoATag] Warning $
                    "Brouken message from PP " ++ show aMsg ++ " " ++ a
                when (not aOk) $ WS.sendTextData aConnect $ A.encode RequestNodeIdToPP

-- TODO class sendMsgToNetLvl
sendMsgToNetLvlFromPP :: ManagerMsg a => Chan a -> MsgToMainActorFromPP -> IO ()
sendMsgToNetLvlFromPP aChan aMsg = writeChan aChan $ msgFromPP aMsg
