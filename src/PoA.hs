{-# LANGUAGE LambdaCase #-}
module PoA (
  servePoA
  )  where

import              System.Clock
import              Data.Hex
import qualified    Data.Serialize as S
import              Control.Monad (forM_, void)
import              Network.Socket.ByteString(sendAllTo)
import              Service.Network.Base
import              Service.Network.UDP.Client
import              Service.Network.UDP.Server
import              Control.Concurrent.Chan
import              Node.Node.Types
import              Service.InfoMsg
import              Service.Types
import              Node.Extra

import              Node.Data.Data
import              Node.Data.NodeTypes

writeLog :: String -> String -> IO ()
writeLog aPath aString = do
    aTime <- getTime Realtime
    appendFile
        ("./data/log_" ++ aPath ++ "_.txt")
        ("["++ show aTime ++ "] " ++ aString ++ "\n")

whenLeft :: (Show a, Show b) => String -> Either a b -> IO ()
whenLeft aPath aMsg@(Left _) = writeLog aPath $ show aMsg
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
    \aMsg aSockAddr _ -> do
        writeLog (show aRecivePort) $ "PaA msg: " ++ (show $ hex $ aMsg)
        let aDecodeMsg = S.decode aMsg
        whenLeft (show aRecivePort) aDecodeMsg
        whenRight aDecodeMsg $ \case
            HashMsgTransactionsRequest num -> do
                writeLog (show aRecivePort) $ "Recived HashMsgTransactionsRequest " ++ show num
                recvTx aSockAddr num
            MBlock mb -> do
                writeLog (show aRecivePort) $ "Recived MBlock \n" ++ show mb
                writeChan ch $ BlockMadeMsg mb
  where
    recvTx aSockAddr aNum =
        runClient (sockAddrToHostAddress aSockAddr) aSendPort $
        \aHandle -> forM_ [1..aNum] $ \_  -> do
            aTransaction <- readChan aRecvChan
            writeChan aInfoChan $ Metric $ add
                ("net.node." ++ show (toInteger aNodeId) ++ ".pending.amount")
                (-1 :: Integer)
            writeLog (show aRecivePort) $  "sendTransaction to poa " ++ show aTransaction
            sendTransaction aHandle aTransaction

-- | Send one transaction.
sendTransaction :: ClientHandle -> Transaction -> IO ()
sendTransaction aHandle aTransaction  = void $ sendAllTo
    (clientSocket aHandle) (S.encode aTransaction) (clientAddress aHandle)
