{-# LANGUAGE LambdaCase #-}
module PoA (
  servePoA
  )  where


import              Data.Hex
import qualified    Data.Serialize as S
import              Control.Monad (forM_, void)
import              Network.Socket.ByteString(sendAllTo, sendAll)
import              Service.Network.Base
import              Service.Network.TCP.Client
import              Service.Network.TCP.Server
import              Control.Concurrent.Chan
import              Node.Node.Types
import              Service.InfoMsg
import              Service.Types
import              Node.Extra

import              Data.Aeson as A

import              Node.Data.Data
import              Node.Data.NodeTypes
import              Node.Data.GlobalLoging
import              PoA.Types
{-
writeLog :: String -> String -> IO ()
writeLog aPath aString = do
    aTime <- getTime Realtime
    appendFile
        ("./data/log_" ++ aPath ++ "_.txt")
        ("["++ show aTime ++ "] " ++ aString ++ "\n")
-}
whenLeft :: (Show a, Show b) => Chan InfoMsg -> Either a b -> IO ()
whenLeft aChan  aMsg@(Left _) = writeLog aChan [ServePoATag] Error $ show aMsg
whenLeft _ _ = pure ()

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
        sendAll aSocket $ A.encode RequestUUIDToPoAPoWMessage
        aId <- newEmptyMVar
        -- writeChan ch $ connecting to PoA, the PoA have id.
        void $ race (aSender aId) (aReceiver aId)
  where
    aSender

    aReceiver

{-
    \aMsg aSockAddr _ -> do
        writeLog aInfoChan [ServePoATag] Info $ "PaA msg: " ++ (show $ hex $ aMsg)
        let aDecodeMsg = S.decode aMsg
        whenLeft aInfoChan aDecodeMsg
        whenRight aDecodeMsg $ \case
            HashMsgTransactionsRequest num -> do
                writeLog aInfoChan [ServePoATag] Info $ "Recived HashMsgTransactionsRequest " ++ show num
                recvTx aSockAddr num
            MBlock mb -> do
                writeLog aInfoChan [ServePoATag] Info $ "Recived MBlock \n" ++ show mb
                writeChan ch $ BlockMadeMsg mb
  where
    recvTx aSockAddr aNum =
        runClient (sockAddrToHostAddress aSockAddr) aSendPort $
        \aHandle -> forM_ [1..aNum] $ \_  -> do
            aTransaction <- readChan aRecvChan
            writeChan aInfoChan $ Metric $ add
                ("net.node." ++ show (toInteger aNodeId) ++ ".pending.amount")
                (-1 :: Integer)
            writeLog aInfoChan [ServePoATag] Info $  "sendTransaction to poa " ++ show aTransaction
            sendTransaction aHandle aTransaction
-}
-- | Send one transaction.
sendTransaction :: ClientHandle -> Transaction -> IO ()
sendTransaction aHandle aTransaction  = void $ sendAllTo
    (clientSocket aHandle) (S.encode aTransaction) (clientAddress aHandle)

{-

\aSocket -> forever $ do
   (aMsg, addr) <- recvFrom aSocket (1024*100)

socketActor
    ::  ManagerMsg a
    =>  HostAddress
    ->  NodeId
    ->  Chan a
    ->  Chan MsgToSender
    ->  WS.Connection
    ->  IO ()
socketActor _ aId aChan aInputChan aConnect = do
    (void $ race sender receiver) `finally`
        (writeChan aChan $ clientIsDisconnected aId aInputChan)
  where
    sender :: IO ()
    sender = readChan aInputChan >>= \case
        MsgToSender aMsg  -> do
            WS.sendBinaryData aConnect aMsg >> sender
        SenderExit aMsg   -> do
            WS.sendBinaryData aConnect aMsg
        SenderTerminate -> pure ()

    receiver :: IO ()
    receiver = forever $ do
        aMsg <- WS.receiveDataMessage aConnect
        writeChan aChan $ datagramMsg (WS.fromDataMessage aMsg) aId
-}
