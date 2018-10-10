{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Enecuum.Legacy.CLI.RPC (serveRpc) where

import           Control.Concurrent.Chan.Unagi.Bounded             (InChan)
import           Control.Monad                                     (forever)
import           Control.Monad.Except                              (throwError)
import           Control.Monad.IO.Class                            (liftIO)
import           Data.IP                                           (AddrRange,
                                                                    IPv6)
import           Data.Maybe                                        (fromMaybe)
import           Data.Text                                         (pack)
import qualified Enecuum.Legacy.CLI.Common                         as C
import           Enecuum.Legacy.Node.Node.Types                    (MsgToCentralActor)
import           Enecuum.Legacy.Refact.Crypto.PublicPrivateKeyPair (Amount,
                                                                    PublicKey)
import           Enecuum.Legacy.Service.Network.WebSockets.Server  (runServer)
import           Enecuum.Legacy.Service.Types                      (ChainInfo,
                                                                    Common (..),
                                                                    DBPoolDescriptor,
                                                                    FullChain,
                                                                    Hash (..),
                                                                    HashOfKeyBlock,
                                                                    HashOfMicroblock,
                                                                    HashOfTransaction,
                                                                    InContainerChan,
                                                                    InfoMsg (..),
                                                                    MacroblockAPI,
                                                                    MacroblockBD,
                                                                    MicroblockAPI,
                                                                    MicroblockBD,
                                                                    MsgTo,
                                                                    Transaction (..),
                                                                    TransactionAPI,
                                                                    TransactionInfo)
import           Network.JsonRpc.Server                            ((:+:) (..), Parameter (..),
                                                                    RpcResult,
                                                                    call,
                                                                    rpcError,
                                                                    toMethod)
import           Network.Socket                                    (PortNumber)
import qualified Network.WebSockets                                as WS
import           Prelude


serveRpc
    :: DBPoolDescriptor
    -> PortNumber
    -> [AddrRange IPv6]
    -> InChan MsgToCentralActor
    -> InChan InfoMsg
    -> InContainerChan
    -> IO ()
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

        handle f = do
            liftIO $ print "Accepted"
            mTx <- liftIO $ f
            case mTx of
                Left  e -> throwError $ rpcError 400 $ pack $ show e
                Right r -> liftIO $ return r


        methods =
            [ createTx
            , balanceReq
            , getBlock
            , getMicroblock
            , getTransaction
            , getFullWallet
            , getPartWallet
            , getSystemInfo
            , getAllChainF
            , getAllLedgerF
            , getAllMicroblocksF
            , getAllKblocksF
            , getAllTransactionsF
            , sendMsgBroadcast
            , sendMsgTo
            , loadMsg
            ]


        createTx = toMethod "enq_sendTransaction" f (Required "tx" :+: ())
          where
            f :: Transaction -> RpcResult IO Hash
            f tx = handle $ C.sendTrans tx ch aInfoCh

        balanceReq = toMethod "enq_getBalance" f (Required "address" :+: ())
          where
            f :: PublicKey -> RpcResult IO Amount
            f key = handle $ C.getBalance descrDB key aInfoCh

        getBlock = toMethod "enq_getBlockByHash" f (Required "hash" :+: ())
          where
            f :: Hash -> RpcResult IO MacroblockAPI
            f hash = handle $ C.getKeyBlockByHash (Common descrDB aInfoCh) hash

        getMicroblock = toMethod "enq_getMicroblockByHash" f (Required "hash" :+: ())
          where
            f :: Hash -> RpcResult IO MicroblockAPI
            f hash = handle $ C.getBlockByHash (Common descrDB aInfoCh) hash

        getTransaction = toMethod "enq_getTransactionByHash" f (Required "hash" :+: ())
          where
            f :: Hash -> RpcResult IO TransactionInfo
            f hash = handle $ C.getTransactionByHash (Common descrDB aInfoCh) hash

        getFullWallet = toMethod "enq_getAllTransactionsByWallet" f (Required "address" :+: ())
          where
            f :: PublicKey -> RpcResult IO [TransactionAPI]
            f key = handle $ C.getAllTransactionsByWallet (Common descrDB aInfoCh) key

        getPartWallet = toMethod "enq_getTransactionsByWallet"
                                 f
                                 (Required "address" :+: Required "offset" :+: Required "count" :+: ())
          where
            f :: PublicKey -> Int -> Int -> RpcResult IO [TransactionAPI]
            f key offset cnt = handle $ C.getPartTransactions (Common descrDB aInfoCh) aContChan key offset cnt

        getSystemInfo = toMethod "enq_getChainInfo" f ()
          where
            f :: RpcResult IO ChainInfo
            f = handle $ C.getChainInfo (Common descrDB aInfoCh)

        getAllChainF = toMethod "enq_getAllChain" f ()
          where
            f :: RpcResult IO [FullChain]
            f = handle $ C.getAllChain (Common descrDB aInfoCh)

        getAllLedgerF = toMethod "enq_getAllLedger" f ()
          where
            f :: RpcResult IO [(PublicKey, Amount)]
            f = handle $ C.getAllLedger (Common descrDB aInfoCh)

        getAllMicroblocksF = toMethod "enq_getAllMicroblocks" f ()
          where
            f :: RpcResult IO [(HashOfMicroblock, MicroblockBD)]
            f = handle $ C.getAllMicroblocks (Common descrDB aInfoCh)

        getAllKblocksF = toMethod "enq_getAllKblocks" f ()
          where
            f :: RpcResult IO [(HashOfKeyBlock, MacroblockBD)]
            f = handle $ C.getAllKblocks (Common descrDB aInfoCh)

        getAllTransactionsF = toMethod "enq_getAllTransactions" f ()
          where
            f :: RpcResult IO [(HashOfTransaction, TransactionInfo)]
            f = handle $ C.getAllTransactions (Common descrDB aInfoCh)
        sendMsgBroadcast = toMethod "send_message_broadcast" f (Required "x" :+: ())
          where
            f :: String -> RpcResult IO ()
            f m = handle $ C.sendMessageBroadcast m ch

        sendMsgTo = toMethod "send_message_to" f (Required "x" :+: ())
          where
            f :: MsgTo -> RpcResult IO ()
            f m = handle $ C.sendMessageTo m ch

        loadMsg = toMethod "load_messages" f ()
          where
            f :: RpcResult IO [MsgTo]
            f = handle $ C.loadMessages ch
