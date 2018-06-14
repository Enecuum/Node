{-# LANGUAGE OverloadedStrings, LambdaCase, FlexibleContexts #-}

module CLI.RPC (serveRpc) where

import Network.Socket (PortNumber)
import Network.JsonRpc.Server
import Network.Socket.ByteString (sendAllTo, recvFrom)
import Service.Network.TCP.Server
import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Monad.Except (throwError)
import Control.Concurrent.Chan
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Maybe (fromMaybe)
import System.IO.Unsafe (unsafePerformIO)

import Data.IP
import CLI.Common
import Node.Node.Types
import Service.Types.SerializeJSON ()
import Service.Types.PublicPrivateKeyPair
import Service.InfoMsg
import Service.Types
import Data.Text (pack)
import Network.Socket (SockAddr)
import Service.Transaction.Storage (DBdescriptor(..))


serveRpc :: DBdescriptor -> PortNumber -> [AddrRange IPv6] -> Chan ManagerMiningMsgBase -> Chan InfoMsg -> IO ()
serveRpc descrDB portNum ipRangeList ch aInfoCh = runServer portNum $ \aSocket -> forever $ do
    (aMsg, addr) <- recvFrom aSocket (1024*100)
    runRpc addr aSocket aMsg

      where
        runRpc addr aSocket aMsg = do
         response <- fromMaybe "" <$> (call methods (fromStrict aMsg))

         sendAllTo aSocket (toStrict response) addr

            where
              ipAccepted :: SockAddr -> Bool
              ipAccepted addr = unsafePerformIO $ do 
                case fromSockAddr addr of
                  Nothing      -> return False
                  Just (ip, _) -> do
                         putStrLn $ "Connection from: " ++ show ip
                         return $ foldl (\p ip_r -> p || isMatchedTo (convert ip) ip_r) False ipRangeList
                    where convert ip = case ip of
                             IPv4 i -> ipv4ToIPv6 i
                             IPv6 i -> i
              
              handle f = do  
                    case ipAccepted addr of
                          False -> do
                                liftIO $ putStrLn "Denied"
                                throwError $ rpcError 401 $ pack "Access denied: wrong IP"
                          True  -> do
                                liftIO $ putStrLn "Accepted"
                                mTx <- liftIO $ f
                                case mTx of
                                     Left e  -> throwError $ rpcError 400 $ pack $ show e
                                     Right r -> liftIO $ return r


              methods = [createTx , balanceReq, getBlock, getTransaction, getFullWallet 
-- test
                       , createNTx, createUnlimTx, sendMsgBroadcast, sendMsgTo, loadMsg 
                        ]


              createTx = toMethod "enq_sendTransaction" f (Required "tx" :+: ()) 
                where
                  f :: Transaction -> RpcResult IO ()
                  f tx = handle $ sendTrans tx ch aInfoCh

              balanceReq = toMethod "enq_getBalance" f (Required "address" :+: ())
                where
                  f :: PubKey -> RpcResult IO Amount
                  f key = handle $ getBalance descrDB key aInfoCh

              getBlock = toMethod "enq_getBlockByHash" f (Required "hash" :+: ())
                where
                  f :: Hash ->  RpcResult IO Microblock
                  f hash = handle $ getBlockByHash hash ch

              getTransaction = toMethod "enq_getTransactionByHash" f (Required "hash" :+:())
                where
                  f :: Hash -> RpcResult IO TransactionInfo
                  f hash = handle $ getTransactionByHash hash ch

              getFullWallet = toMethod "enq_getAllTransactions" f (Required "address" :+: ())
                where
                  f :: PubKey -> RpcResult IO [Transaction]
                  f key = handle $ getAllTransactions key ch

------------- test functions
              createNTx = toMethod "gen_n_tx" f (Required "x" :+: ())
                where
                  f :: Int -> RpcResult IO ()
                  f num = handle $ generateNTransactions num ch aInfoCh

              createUnlimTx = toMethod "gen_unlim_tx" f ()
                where
                  f :: RpcResult IO ()
                  f = handle $ generateTransactionsForever ch aInfoCh

              sendMsgBroadcast = toMethod "send_message_broadcast" f (Required "x" :+: ())
                where
                  f :: String -> RpcResult IO ()
                  f m = handle $ sendMessageBroadcast m ch

              sendMsgTo = toMethod "send_message_to" f (Required "x" :+: ())
                where
                  f :: MsgTo -> RpcResult IO ()
                  f m = handle $ sendMessageTo m ch

              loadMsg = toMethod "load_messages" f ()
                where
                  f :: RpcResult IO [MsgTo]
                  f = handle $ loadMessages ch


