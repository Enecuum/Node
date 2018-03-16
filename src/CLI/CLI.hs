{-# LANGUAGE OverloadedStrings #-}

module CLI.CLI (control) where

import Network.JsonRpc.Server
import Network.Socket.ByteString (sendAllTo)
import Service.Network.UDP.Server
import Control.Monad (forever, replicateM, forM)
import Control.Monad.IO.Class
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.Chan
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Time.Units
import Data.Graph.Inductive (labEdges)
import Data.Maybe (fromMaybe)
import System.Random (randomRIO)

import CLI.Balance
import CLI.TransactionsDAG
import Node.Node.Types
import Service.Types
import Service.Types.SerializeJSON ()
import Service.Types.PublicPrivateKeyPair
import Service.System.Directory
import Service.Metrics

data TxChanMsg = NewTx Transaction 
               | GenTxNum Int
               | GenTxUnlim


control :: String -> Chan ManagerMiningMsgBase -> IO ()
control portNum ch = runServer (read portNum) $ \aMsg addr aSocket -> do
    txChan     <- newChan
    ledgerRespChan <- newChan
    ledgerReqChan <- newChan
    forkIO $ txWait txChan ch
    forkIO $ ledgerWait ledgerReqChan ledgerRespChan
    runRpc txChan ledgerReqChan ledgerRespChan addr aSocket aMsg
      where
        runRpc txChan ledgerReqChan ledgerRespChan addr aSocket aMsg = do
          response <- call methods (fromStrict aMsg)
          sendAllTo aSocket (toStrict $ fromMaybe "" response) addr
            where
              methods = [createTx , createNTx, createUnlimTx, balanceReq ]

              createTx = toMethod "new_tx" f (Required "x" :+: ())
                where
                  f :: Transaction -> RpcResult IO ()
                  f tx = liftIO $ do
                      sendMetrics tx
                      writeChan txChan $ NewTx tx

              createNTx = toMethod "gen_n_tx" f (Required "x" :+: ())
                where
                  f :: Int -> RpcResult IO ()
                  f num = liftIO $ writeChan txChan $ GenTxNum num

              createUnlimTx = toMethod "gen_unlim_tx" f ()
                where
                  f :: RpcResult IO ()
                  f = liftIO $ writeChan txChan GenTxUnlim

              balanceReq = toMethod "get_balance" f (Required "x" :+: ())
                where
                  f :: PublicKey -> RpcResult IO Amount
                  f key = liftIO $ do
                    writeChan ledgerReqChan key
                    resp <- readChan ledgerRespChan
                    return resp



txWait :: Chan TxChanMsg -> Chan ManagerMiningMsgBase -> IO ()
txWait recvCh mngCh = do
    msg <- readChan recvCh
    case msg of
      NewTx tx       -> writeChan mngCh $ newTransaction tx
      GenTxNum num   -> generateNTransactions num   mngCh
      GenTxUnlim     -> generateTransactionsForever mngCh
    txWait recvCh mngCh


ledgerWait :: Chan PublicKey -> Chan Amount -> IO ()
ledgerWait chReq chResp = do
    key <- readChan chReq
    stTime  <- ( getCPUTimeWithUnit :: IO Millisecond )
    result  <- countBalance key
    endTime <- ( getCPUTimeWithUnit :: IO Millisecond )
    metric $ timing "cl.ld.time" (subTime stTime endTime)
    writeChan chResp result
    ledgerWait chReq chResp



genTxDAG :: Int -> IO [Transaction]
genTxDAG n = do
   keys <- replicateM n generateNewRandomAnonymousKeyPair
   dag <- getTransactionDAG keys
   forM (labEdges dag)  $ \(_, _, tr) -> return tr

generateNTransactions :: ManagerMiningMsg a =>
    Int -> Chan a -> IO ()
generateNTransactions qTx ch = do
  tx <- genTxDAG qTx
  mapM_ (\x -> do
          writeChan ch $ newTransaction x
          sendMetrics x
        ) tx
  putStrLn "Transactions are created"


generateTransactionsForever :: ManagerMiningMsg a => Chan a -> IO b
generateTransactionsForever ch = forever $ do
                                quantityOfWallets <- randomRIO (20,30)
                                tx <- genTxDAG quantityOfWallets
                                mapM_ (\x -> do
                                            writeChan ch $ newTransaction x
                                            sendMetrics x
                                       ) tx
                                threadDelay (10^(6 :: Int))
                                putStrLn ("Bundle of " ++ show quantityOfWallets ++"Transactions was created")

sendMetrics :: Transaction -> IO ()
sendMetrics (WithTime _ tx) = sendMetrics tx
sendMetrics (WithSignature tx _) = sendMetrics tx
sendMetrics (RegisterPublicKey k b) = do
                           metric $ increment "cl.tx.count"
                           metric $ set "cl.tx.wallet" k
                           metric $ gauge "cl.tx.amount" b
sendMetrics (SendAmountFromKeyToKey o r a) = do
                           metric $ increment "cl.tx.count"
                           metric $ set "cl.tx.wallet" o
                           metric $ set "cl.tx.wallet" r
                           metric $ gauge "cl.tx.amount" a
