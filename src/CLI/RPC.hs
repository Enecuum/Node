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

import           CLI.Common
import           Data.IP
import           Data.Text                             (pack)
import           Network.Socket                        (PortNumber)
import qualified Network.WebSockets                    as WS
import           Node.Node.Types
import           Service.InfoMsg
import           Service.Types
import           Service.Types.PublicPrivateKeyPair
import           Service.Types.SerializeJSON           ()


-- type OffsetMap = M.Map (PublicKey, Int) Rocks.Iterator



serveRpc :: DBPoolDescriptor -> PortNumber -> [AddrRange IPv6] -> InChan MsgToCentralActor -> InChan InfoMsg -> InContainerChan -> IO ()
serveRpc descrDB portNum _ ch aInfoCh aContChan = runServer portNum "serveRpc" $ \_ aPending -> do
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
              -- ipAccepted :: SockAddr -> Bool
              -- ipAccepted addr = unsafePerformIO $
              --   case fromSockAddr addr of
              --     Nothing      -> return False
              --     Just (ip, _) -> do
              --            putStrLn $ "Connection from: " ++ show ip
              --            return $ foldl (\p ip_r -> p || isMatchedTo (convert ip) ip_r) False ipRangeList
              --       where convert ip = case ip of
              --                IPv4 i -> ipv4ToIPv6 i
              --                IPv6 i -> i

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
                       , getAllChainF, getAllLedgerF, getAllMicroblocksF, getAllKblocksF, getAllTransactionsF
-- test
--                       , createNTx, createUnlimTx, sendMsgBroadcast, sendMsgTo, loadMsg
                        ]


              createTx = toMethod "enq_sendTransaction" f (Required "tx" :+: ())
                where
                  f :: Transaction -> RpcResult IO Hash
                  f tx = handle $ sendTrans tx ch aInfoCh

              balanceReq = toMethod "enq_getBalance" f (Required "address" :+: ())
                where
                  f :: PublicKey -> RpcResult IO Amount
                  f key = handle $ getBalance descrDB key aInfoCh

              getBlock = toMethod "enq_getBlockByHash" f (Required "hash" :+: ())
                where
                  f :: Hash ->  RpcResult IO MacroblockAPI
                  f hash = handle $ getKeyBlockByHash (Common descrDB aInfoCh) hash

              getMicroblock = toMethod "enq_getMicroblockByHash" f (Required "hash" :+: ())
                where
                  f :: Hash ->  RpcResult IO MicroblockAPI
                  f hash = handle $ getBlockByHash (Common descrDB aInfoCh) hash

              getTransaction = toMethod "enq_getTransactionByHash" f (Required "hash" :+:())
                where
                  f :: Hash -> RpcResult IO TransactionInfo
                  f hash = handle $ getTransactionByHash (Common descrDB aInfoCh) hash

              getFullWallet = toMethod "enq_getAllTransactionsByWallet" f (Required "address" :+: ())
                where
                  f :: PublicKey -> RpcResult IO [TransactionAPI]
                  f key = handle $ getAllTransactionsByWallet (Common descrDB aInfoCh) key

              getPartWallet = toMethod "enq_getTransactionsByWallet" f (Required "address" :+: Required "offset" :+: Required "count" :+: ())
                where
                  f :: PublicKey -> Int -> Int -> RpcResult IO [TransactionAPI]
                  f key offset cnt = handle $ getPartTransactions (Common descrDB aInfoCh) aContChan key offset cnt

              getSystemInfo = toMethod "enq_getChainInfo" f ()
                where
                  f :: RpcResult IO ChainInfo
                  f = handle $ getChainInfo (Common descrDB aInfoCh)

              getAllChainF = toMethod "enq_getAllChain" f ()
                where
                  f :: RpcResult IO [FullChain]
                  f = handle $ getAllChain (Common descrDB aInfoCh)

              getAllLedgerF = toMethod "enq_getAllLedger" f ()
                where
                  f :: RpcResult IO [(PublicKey, Amount)]
                  f = handle $ getAllLedger (Common descrDB aInfoCh)

              getAllMicroblocksF = toMethod "enq_getAllMicroblocks" f ()
                where
                  f :: RpcResult IO [(HashOfMicroblock, MicroblockBD)]
                  f = handle $ getAllMicroblocks (Common descrDB aInfoCh)

              getAllKblocksF = toMethod "enq_getAllKblocks" f ()
                where
                  f :: RpcResult IO [(HashOfKeyBlock, MacroblockBD)]
                  f = handle $ getAllKblocks (Common descrDB aInfoCh)

              getAllTransactionsF = toMethod "enq_getAllTransactions" f ()
                where
                  f :: RpcResult IO [(HashOfTransaction, TransactionInfo)]
                  f = handle $ getAllTransactions (Common descrDB aInfoCh)

------------- test functions
              -- deleteAllDB = toMethod "enq_deleteAllDB" f ()
              --   where
              --     f :: RpcResult IO ()
              --     f = handle $ deleteAllDB (Common descrDB aInfoCh)

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
