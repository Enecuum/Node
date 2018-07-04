{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI.RPC (serveRpc) where

import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Monad                         (forever)
import           Control.Monad.Except                  (throwError)
import           Control.Monad.IO.Class
import           Data.Maybe                            (fromMaybe)
import           Network.JsonRpc.Server
import           Service.Network.WebSockets.Server
import           System.IO.Unsafe                      (unsafePerformIO)

import           CLI.Common
import           Data.IP
import           Data.Text                             (pack)
import           Network.Socket                        (PortNumber, SockAddr)
import qualified Network.WebSockets                    as WS
import           Node.Node.Types
import           Service.InfoMsg
import           Service.Transaction.Storage           (DBPoolDescriptor (..))
import           Service.Types
import           Service.Types.PublicPrivateKeyPair
import           Service.Types.SerializeJSON           ()


serveRpc :: DBPoolDescriptor -> PortNumber -> [AddrRange IPv6] -> InChan ManagerMiningMsgBase -> InChan InfoMsg -> IO ()
serveRpc descrDB portNum ipRangeList ch aInfoCh = runServer portNum $ \_ aPending -> do
    aConnect <- WS.acceptRequest aPending
    WS.forkPingThread aConnect 30
    forever $ do
      aMsg <- WS.receiveData aConnect
      runRpc aConnect aMsg

     where
        runRpc aConnect aMsg = do
         response <- fromMaybe "" <$> call methods aMsg
         WS.sendTextData aConnect response

            where
              ipAccepted :: SockAddr -> Bool
              ipAccepted addr = unsafePerformIO $
                case fromSockAddr addr of
                  Nothing      -> return False
                  Just (ip, _) -> do
                         putStrLn $ "Connection from: " ++ show ip
                         return $ foldl (\p ip_r -> p || isMatchedTo (convert ip) ip_r) False ipRangeList
                    where convert ip = case ip of
                             IPv4 i -> ipv4ToIPv6 i
                             IPv6 i -> i

              handle f =
                    case {-ipAccepted addr-} True of
                          False -> do
                                liftIO $ putStrLn "Denied"
                                throwError $ rpcError 401 $ pack "Access denied: wrong IP"
                          True  -> do
                                liftIO $ putStrLn "Accepted"
                                mTx <- liftIO $ f
                                case mTx of
                                     Left e  -> throwError $ rpcError 400 $ pack $ show e
                                     Right r -> liftIO $ return r


              methods = [createTx , balanceReq, getBlock, getMicroblock
                       , getTransaction, getFullWallet, getPartWallet, getSystemInfo
-- test
                       , createNTx, createUnlimTx, sendMsgBroadcast, sendMsgTo, loadMsg
                        ]


              createTx = toMethod "enq_sendTransaction" f (Required "tx" :+: ())
                where
                  f :: Transaction -> RpcResult IO ()
                  f tx = handle $ sendTrans tx ch aInfoCh

              balanceReq = toMethod "enq_getBalance" f (Required "address" :+: ())
                where
                  f :: PublicKey -> RpcResult IO Amount
                  f key = handle $ getBalance descrDB key aInfoCh

              getBlock = toMethod "enq_getBlockByHash" f (Required "hash" :+: ())
                where
                  f :: Hash ->  RpcResult IO MacroblockAPI
                  f hash = handle $ getKeyBlockByHash descrDB hash ch

              getMicroblock = toMethod "enq_getMicroblockByHash" f (Required "hash" :+: ())
                where
                  f :: Hash ->  RpcResult IO MicroblockAPI
                  f hash = handle $ getBlockByHash descrDB hash ch

              getTransaction = toMethod "enq_getTransactionByHash" f (Required "hash" :+:())
                where
                  f :: Hash -> RpcResult IO TransactionInfo
                  f hash = handle $ getTransactionByHash descrDB hash ch

              getFullWallet = toMethod "enq_getAllTransactions" f (Required "address" :+: ())
                where
                  f :: PublicKey -> RpcResult IO [TransactionAPI]
                  f key = handle $ getAllTransactions descrDB key ch

              getPartWallet = toMethod "enq_getTransactionsByWallet" f (Required "address" :+: Required "offset" :+: Required "count" :+: ())
                where
                  f :: PublicKey -> Int -> Int -> RpcResult IO [TransactionAPI]
                  f key offset cnt = handle $ getPartTransactions descrDB key offset cnt ch

              getSystemInfo = toMethod "enq_getChainInfo" f ()
                where
                  f :: RpcResult IO ChainInfo
                  f = handle $ getChainInfo ch

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
