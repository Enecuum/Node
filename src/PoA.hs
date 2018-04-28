{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
module PoA (
  servePoA
  )  where


import              Node.Data.NetPackage
import              Data.Hex
import qualified    Data.Serialize as S
import              Control.Monad (forM_, void, forever)
import              Network.Socket.ByteString(sendAllTo, sendAll, recvFrom)
import              Service.Network.Base
import              Service.Network.TCP.Client
import              Service.Network.TCP.Server
import              Control.Concurrent.Chan
import              Node.Node.Types
import              Service.InfoMsg as I
import              Service.Types
import              Node.Extra
import              System.Random.Shuffle
import              Data.String

import              Data.Aeson as A

import              Node.Node.Base
import              Node.Data.Data
import              Node.Data.NodeTypes
import              Node.Data.GlobalLoging
import              PoA.Types
import              Control.Concurrent.MVar
import              Control.Concurrent

import              Control.Concurrent.Async

whenLeft :: (Show a, Show b) => Chan InfoMsg -> Either a b -> IO ()
whenLeft aChan  aMsg@(Left _) = writeLog aChan [ServePoATag] I.Error $ show aMsg
whenLeft _ _ = pure ()

myDecode = A.decode.fromString.show
myEncode = fromString.show.A.encode

servePoA ::
       PortNumber
    -> PortNumber
    -> MyNodeId
    -> Chan ManagerMiningMsgBase
    -> Chan Transaction
    -> Chan InfoMsg
    -> IO ()
servePoA aRecivePort aSendPort aNodeId ch aRecvChan aInfoChan = runServer aRecivePort $
    \aSocket -> do
        sendAll aSocket $ myEncode RequestUUIDToPP
        aId <- newEmptyMVar
        aNewChan  <- newChan
        -- writeChan ch $ connecting to PoA, the PoA have id.
        void $ race
            (aSender aId aSocket aNewChan)
            (aReceiver aId aSocket aNewChan)
  where
    aSender aId aSocket aNewChan = forever $ do
        aMsg <- readChan aNewChan
        sendAll aSocket $ myEncode aMsg

    aReceiver aId aSocket aNewChan = do
        (aMsg, _) <- recvFrom aSocket (1024*100)
        aOk <- isEmptyMVar aId
        case myDecode $ aMsg of
            Just a -> case a of
                -- REVIEW: Check fair distribution of transactions between nodes
                RequestTransaction aNum -> void $ forkIO $ forM_ [1..aNum] $ \_  -> do
                        aTransaction <- readChan aRecvChan
                        writeChan aInfoChan $ Metric $ add
                            ("net.node." ++ show (toInteger aNodeId) ++ ".pending.amount")
                            (-1 :: Integer)
                        writeLog aInfoChan [ServePoATag] Info $  "sendTransaction to poa " ++ show aTransaction
                        sendAll aSocket $ fromString.show $ A.encode $ ResponseTransaction aTransaction

                MsgMicroblock aMicroblock -> do
                    writeLog aInfoChan [ServePoATag] Info $ "Recived MBlock: " ++ show aMicroblock
                    sendMsgToNetLvlFromPP ch $ MicroblockFromPP aMicroblock

                RequestBroadcast recipientType msg
                    | not aOk -> do
                        aNodeId <- readMVar aId
                        writeLog aInfoChan [ServePoATag] Info $ "Broadcast request " ++ show aMsg
                        sendMsgToNetLvlFromPP ch $ BroadcastRequestFromPP msg (IdFrom aNodeId) recipientType
                    | otherwise -> do
                        writeLog aInfoChan [ServePoATag] Warning $ "Broadcast request  without UUID " ++ show aMsg
                        sendAll aSocket $ myEncode RequestUUIDToPP
                RequestConnects -> do
                    NodeInfoListNetLvl aRecords <- readRecordsFromNodeListFile
                    aShuffledRecords <- shuffleM aRecords
                    let aConnects = (\(_, a, b) -> Connect a b) <$> (take 5 aShuffledRecords)
                    writeLog aInfoChan [ServePoATag] Info $ "Send connections " ++ show aConnects
                    sendAll aSocket $ myEncode $ ResponseConnects aConnects

                ResponseUUIDToNN aUuid aNodeType | aOk -> do
                    putMVar aId aUuid
                    writeLog aInfoChan [ServePoATag] Info $ "Accept UUID " ++ show aUuid ++ " with type " ++ show aNodeType

                    sendMsgToNetLvlFromPP ch $ NewConnectWithPP aUuid aNodeType aNewChan

                MsgMsgToNN aDestination aMsg
                    | not aOk       -> do
                        aNodeId <- readMVar aId
                        writeLog aInfoChan [ServePoATag] Info $
                            "Resending the msg from " ++ show aNodeId ++ " the msg is " ++ show aMsg
                        sendMsgToNetLvlFromPP ch $ MsgResendingToPP (IdFrom aNodeId) (IdTo aDestination) aMsg
                    | otherwise     -> do
                        writeLog aInfoChan [ServePoATag] Warning $ "Can't send request without UUID " ++ show aMsg
                        sendAll aSocket $ myEncode RequestUUIDToPP

            Nothing -> do
                -- TODO: Вписать ID если такой есть.
                writeLog aInfoChan [ServePoATag] Warning $
                    "Brouken message from PP " ++ show aMsg
-- TODO class sendMsgToNetLvl
sendMsgToNetLvlFromPP aChan aMsg = writeChan aChan $ msgFromPP aMsg
