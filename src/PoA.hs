{-# LANGUAGE LambdaCase #-}
module PoA (
  servePoA
  )  where

import              System.Clock
import              Data.Hex
import qualified    Data.Serialize as S
import              Control.Monad (forM_, void, when)
import              Network.Socket.ByteString(sendAllTo)
import              Service.Network.Base
import              Service.Network.UDP.Client
import              Service.Network.UDP.Server
import              Control.Concurrent.Chan
import              Node.Node.Types
import              Service.Metrics
import              Service.Types
import              Node.Extra

import              Node.Data.Data

loging :: String -> String -> IO ()
loging aPath aString = do
    aTime <- getTime Realtime
    appendFile
        ("./data/log_" ++ aPath ++ "_.txt")
        ("["++ show aTime ++ "] " ++ aString ++ "\n")

whenLeft :: (Show a, Show b) => String -> Either a b -> IO ()
whenLeft aPath aMsg@(Left _) = loging aPath $ show aMsg
whenLeft _ _ = pure ()

isLeft (Left _) = True
isLeft _        = False

servePoA ::
    String
    -> MyNodeId
    -> Chan ManagerMiningMsgBase
    -> Chan Transaction
    -> String
    -> IO ()
servePoA aRecivePort aNodeId ch aRecvChan aSendPort = runServer (read aRecivePort) $
    \aMsg aSockAddr _ -> do
        let aDecodeMsg = S.decode aMsg
        whenLeft aRecivePort aDecodeMsg
        when (isLeft aDecodeMsg) $
            loging (aRecivePort) $ "PaA msg: " ++ (show $ hex $ aMsg)

        whenRight aDecodeMsg $ \case
            HashMsgTransactionsRequest num -> do
                loging (aRecivePort) $ "Recived HashMsgTransactionsRequest " ++ show num
                recvTx aSockAddr num
            MBlock mb -> do
                loging aRecivePort $ "Recived MBlock \n" ++ show mb
                writeChan ch $ BlockMadeMsg mb
  where
    recvTx aSockAddr aNum =
        runClient (sockAddrToHostAddress aSockAddr) (read aSendPort) $
        \aHandle -> forM_ [1..aNum] $ \_  -> do
            aTransaction <- readChan aRecvChan
            metric $ add
                ("net.node." ++ show (toInteger aNodeId) ++ ".pending.amount")
                (-1 :: Integer)
            loging aRecivePort $  "sendTransaction to poa " ++ show aTransaction
            sendTransaction aHandle aTransaction

-- | Send one transaction.
sendTransaction :: ClientHandle -> Transaction -> IO ()
sendTransaction aHandle aTransaction  = void $ sendAllTo
    (clientSocket aHandle) (S.encode aTransaction) (clientAddress aHandle)
