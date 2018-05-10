{-# LANGUAGE ScopedTypeVariables #-}
module PoA.PoAServer (
        servePoA
    ,   serverPoABootNode
  )  where


import              Node.Data.NetPackage
import              Control.Monad (forM_, void, forever, unless)
import qualified    Network.WebSockets                  as WS
--import              Network.Socket.ByteString(sendAll, recvFrom)
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

myDecode :: B.ByteString -> Maybe PPToNNMessage
myDecode = A.decode.fromString.show

myEncode :: NNToPPMessage -> B.ByteString
myEncode = fromString.show.A.encode

--
serverPoABootNode :: PortNumber -> Chan InfoMsg -> Chan FileActorRequest -> IO ()
serverPoABootNode aRecivePort aInfoChan aFileServerChan = runServer aRecivePort $ \_ aPending -> do
    aConnect <- WS.acceptRequest aPending
    WS.forkPingThread aConnect 30
    aMsg <- WS.receiveData aConnect
    case myDecode aMsg of
        Just a -> case a of
            RequestConnects -> do
                aConChan <- newChan
                writeChan (aFileServerChan) $
                     FileActorRequestNetLvl $ ReadRecordsFromNodeListFile aConChan
                NodeInfoListNetLvl aRecords <- readChan aConChan
                aShuffledRecords <- shuffleM aRecords
                let aConnects = snd <$> take 5 aShuffledRecords
                writeLog aInfoChan [ServerBootNodeTag] Info $ "Send connections " ++ show aConnects
                WS.sendBinaryData aConnect $ myEncode $ ResponseConnects aConnects
            _  -> writeLog aInfoChan [ServerBootNodeTag] Warning $
                "Brouken message from PP " ++ show aMsg
        Nothing ->
            -- TODO: Вписать ID если такой есть.
            writeLog aInfoChan [ServerBootNodeTag] Warning $
                "Brouken message from PP " ++ show aMsg


servePoA ::
       PortNumber
    -> MyNodeId
    -> Chan ManagerMiningMsgBase
    -> Chan Transaction
    -> Chan InfoMsg
    -> Chan FileActorRequest
    -> IO ()
servePoA aRecivePort aNodeId ch aRecvChan aInfoChan aFileServerChan = runServer aRecivePort $
    \_ aPending -> do
        aConnect <- WS.acceptRequest aPending
        WS.forkPingThread aConnect 30

        WS.sendBinaryData aConnect $ myEncode RequestNodeIdToPP
        aId <- newEmptyMVar
        aNewChan  <- newChan
        -- writeChan ch $ connecting to PoA, the PoA have id.
        void $ race
            (aSender aId aConnect aNewChan)
            (aReceiver aId aConnect aNewChan)
  where
    aSender aId aConnect aNewChan = forever (do
        aMsg <- readChan aNewChan
        WS.sendBinaryData aConnect $ myEncode aMsg) `finally` (do
            aIsEmpty <- isEmptyMVar aId
            unless aIsEmpty $ do
                aDeadId <- readMVar aId
                writeChan ch $ ppNodeIsDisconected aDeadId)

    aReceiver aId aConnect aNewChan = do
        aMsg <- WS.receiveData aConnect
        aOk <- isEmptyMVar aId
        case myDecode aMsg of
            Just a -> case a of
                -- REVIEW: Check fair distribution of transactions between nodes
                RequestTransaction aNum -> void $ forkIO $ forM_ [1..aNum] $ \_  -> do
                        aTransaction <- readChan aRecvChan
                        writeChan aInfoChan $ Metric $ add
                            ("net.node." ++ show (toInteger aNodeId) ++ ".pending.amount")
                            (-1 :: Integer)
                        writeLog aInfoChan [ServePoATag] Info $  "sendTransaction to poa " ++ show aTransaction
                        WS.sendBinaryData aConnect
                            (fromString.show $ A.encode $ ResponseTransaction aTransaction :: B.ByteString)
                MsgMicroblock aMicroblock
                    | not aOk -> do
                        aSenderId <- readMVar aId
                        writeLog aInfoChan [ServePoATag] Info $ "Recived MBlock: " ++ show aMicroblock
                        sendMsgToNetLvlFromPP ch $ MicroblockFromPP aMicroblock aSenderId
                    | otherwise -> do
                        writeLog aInfoChan [ServePoATag] Warning $ "Broadcast request  without PPId " ++ show aMsg
                        WS.sendBinaryData aConnect $ myEncode RequestNodeIdToPP

                RequestBroadcast aRecipientType aBroadcastMsg
                    | not aOk -> do
                        aSenderId <- readMVar aId
                        writeLog aInfoChan [ServePoATag] Info $ "Broadcast request " ++ show aMsg
                        sendMsgToNetLvlFromPP ch $
                            BroadcastRequestFromPP aBroadcastMsg (IdFrom aSenderId) aRecipientType
                    | otherwise -> do
                        writeLog aInfoChan [ServePoATag] Warning $ "Broadcast request  without PPId " ++ show aMsg
                        WS.sendBinaryData aConnect $ myEncode RequestNodeIdToPP
                RequestConnects -> do
                    aConChan <- newChan
                    writeChan (aFileServerChan) $
                         FileActorRequestNetLvl $ ReadRecordsFromNodeListFile aConChan
                    NodeInfoListNetLvl aRecords <- readChan aConChan
                    aShuffledRecords <- shuffleM aRecords
                    let aConnects = snd <$> take 5 aShuffledRecords
                    writeLog aInfoChan [ServePoATag] Info $ "Send connections " ++ show aConnects
                    WS.sendBinaryData aConnect $ myEncode $ ResponseConnects aConnects

                RequestPoWList
                    | not aOk -> do
                        aSenderId <- readMVar aId
                        writeLog aInfoChan [ServePoATag] Info $
                            "PoWListRequest the msg from " ++ show aSenderId
                        sendMsgToNetLvlFromPP ch $ PoWListRequest (IdFrom aSenderId)

                    | otherwise -> do
                        writeLog aInfoChan [ServePoATag] Warning "Can't send request without PPId "
                        WS.sendBinaryData aConnect $ myEncode RequestNodeIdToPP


                ResponseNodeIdToNN aPPId aNodeType -> do
                    if aOk then putMVar aId aPPId else void $ swapMVar aId aPPId
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
                        WS.sendBinaryData aConnect $ myEncode RequestNodeIdToPP

            Nothing ->
                -- TODO: Вписать ID если такой есть.
                writeLog aInfoChan [ServePoATag] Warning $
                    "Brouken message from PP " ++ show aMsg

-- TODO class sendMsgToNetLvl
sendMsgToNetLvlFromPP :: ManagerMsg a => Chan a -> MsgToMainActorFromPP -> IO ()
sendMsgToNetLvlFromPP aChan aMsg = writeChan aChan $ msgFromPP aMsg
