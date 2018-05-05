{-# LANGUAGE ScopedTypeVariables #-}
module PoA (
        servePoA
    ,   serverPoABootNode
  )  where


import              Node.Data.NetPackage
import              Data.Hex()
import              Data.Serialize()
import              Control.Monad (forM_, void, forever, unless)
import              Network.Socket.ByteString(sendAll, recvFrom)
import              Service.Network.Base
import              Service.Network.TCP.Client()
import              Service.Network.TCP.Server
import              Control.Concurrent.Chan
import              Node.Node.Types
import              Service.InfoMsg as I
import              Service.Types
import              Node.Extra()
import              System.Random.Shuffle
import              Data.String
import qualified    Data.ByteString as B
import              Data.Aeson as A
import              Control.Exception
import              Node.Node.Base
import              Node.Data.Data()
import              Node.Data.NodeTypes
import              Node.Data.GlobalLoging
import              PoA.Types
import              Control.Concurrent.MVar
import              Control.Concurrent

import              Control.Concurrent.Async

myDecode :: B.ByteString -> Maybe PPToNNMessage
myDecode = A.decode.fromString.show

myEncode :: NNToPPMessage -> B.ByteString
myEncode = fromString.show.A.encode

--
serverPoABootNode :: PortNumber -> Chan InfoMsg -> IO ()
serverPoABootNode aRecivePort aInfoChan = runServer aRecivePort $ \aSocket -> do
    (aMsg, _) <- recvFrom aSocket (1024*100)
    case myDecode aMsg of
        Just a -> case a of
            RequestConnects -> do
                NodeInfoListNetLvl aRecords <- readRecordsFromNodeListFile
                aShuffledRecords <- shuffleM aRecords
                let aConnects = (\(_, b, c) -> Connect b c) <$> take 5 aShuffledRecords
                writeLog aInfoChan [ServerBootNodeTag] Info $ "Send connections " ++ show aConnects
                sendAll aSocket $ myEncode $ ResponseConnects aConnects
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
    -> IO ()
servePoA aRecivePort aNodeId ch aRecvChan aInfoChan = runServer aRecivePort $
    \aSocket -> do
        sendAll aSocket $ myEncode RequestNodeIdToPP
        aId <- newEmptyMVar
        aNewChan  <- newChan
        -- writeChan ch $ connecting to PoA, the PoA have id.
        void $ race
            (aSender aId aSocket aNewChan)
            (aReceiver aId aSocket aNewChan)
  where
    aSender aId aSocket aNewChan = forever (do
        aMsg <- readChan aNewChan
        sendAll aSocket $ myEncode aMsg) `finally` (do
            aIsEmpty <- isEmptyMVar aId
            unless aIsEmpty $ do
                aDeadId <- readMVar aId
                writeChan ch $ ppNodeIsDisconected aDeadId)

    aReceiver aId aSocket aNewChan = do
        (aMsg, _) <- recvFrom aSocket (1024*100)
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
                        sendAll aSocket $ fromString.show $ A.encode $ ResponseTransaction aTransaction

                MsgMicroblock aMicroblock
                    | not aOk -> do
                        aSenderId <- readMVar aId
                        writeLog aInfoChan [ServePoATag] Info $ "Recived MBlock: " ++ show aMicroblock
                        sendMsgToNetLvlFromPP ch $ MicroblockFromPP aMicroblock aSenderId
                    | otherwise -> do
                        writeLog aInfoChan [ServePoATag] Warning $ "Broadcast request  without UUID " ++ show aMsg
                        sendAll aSocket $ myEncode RequestNodeIdToPP

                RequestBroadcast aRecipientType aBroadcastMsg
                    | not aOk -> do
                        aSenderId <- readMVar aId
                        writeLog aInfoChan [ServePoATag] Info $ "Broadcast request " ++ show aMsg
                        sendMsgToNetLvlFromPP ch $
                            BroadcastRequestFromPP aBroadcastMsg (IdFrom aSenderId) aRecipientType
                    | otherwise -> do
                        writeLog aInfoChan [ServePoATag] Warning $ "Broadcast request  without UUID " ++ show aMsg
                        sendAll aSocket $ myEncode RequestNodeIdToPP
                RequestConnects -> do
                    NodeInfoListNetLvl aRecords <- readRecordsFromNodeListFile
                    aShuffledRecords <- shuffleM aRecords
                    let aConnects = (\(_, b, c) -> Connect b c) <$> take 5 aShuffledRecords
                    writeLog aInfoChan [ServePoATag] Info $ "Send connections " ++ show aConnects
                    sendAll aSocket $ myEncode $ ResponseConnects aConnects

                RequestPoWList
                    | not aOk -> do
                        aSenderId <- readMVar aId
                        writeLog aInfoChan [ServePoATag] Info $
                            "PoWListRequest the msg from " ++ show aSenderId
                        sendMsgToNetLvlFromPP ch $ PoWListRequest (IdFrom aSenderId)

                    | otherwise -> do
                        writeLog aInfoChan [ServePoATag] Warning "Can't send request without UUID "
                        sendAll aSocket $ myEncode RequestNodeIdToPP


                ResponseNodeIdToNN aUuid aNodeType -> do
                    if aOk then putMVar aId aUuid else void $ swapMVar aId aUuid
                    writeLog aInfoChan [ServePoATag] Info $
                        "Accept UUID " ++ show aUuid ++ " with type " ++ show aNodeType

                    sendMsgToNetLvlFromPP ch $ NewConnectWithPP aUuid aNodeType aNewChan

                MsgMsgToNN aDestination aMsgToNN
                    | not aOk       -> do
                        aSenderId <- readMVar aId
                        writeLog aInfoChan [ServePoATag] Info $
                            "Resending the msg from " ++ show aSenderId ++ " the msg is " ++ show aMsgToNN
                        sendMsgToNetLvlFromPP ch $ MsgResendingToPP (IdFrom aSenderId) (IdTo aDestination) aMsgToNN
                    | otherwise     -> do
                        writeLog aInfoChan [ServePoATag] Warning $ "Can't send request without UUID " ++ show aMsgToNN
                        sendAll aSocket $ myEncode RequestNodeIdToPP

            Nothing ->
                -- TODO: Вписать ID если такой есть.
                writeLog aInfoChan [ServePoATag] Warning $
                    "Brouken message from PP " ++ show aMsg

-- TODO class sendMsgToNetLvl
sendMsgToNetLvlFromPP :: ManagerMsg a => Chan a -> MsgToMainActorFromPP -> IO ()
sendMsgToNetLvlFromPP aChan aMsg = writeChan aChan $ msgFromPP aMsg
