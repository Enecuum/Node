{-# LANGUAGE OverloadedStrings, LambdaCase #-}

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

import CLI.Common
import Node.Node.Types
import Service.Types.SerializeJSON ()
import Service.Types.PublicPrivateKeyPair
import Service.InfoMsg
import Service.Types

serveRpc :: PortNumber -> Chan ManagerMiningMsgBase -> Chan InfoMsg -> IO ()
serveRpc portNum ch aInfoCh = runServer portNum $ \aSocket -> forever $ do
    (aMsg, addr) <- recvFrom aSocket (1024*100)
    runRpc addr aSocket aMsg
      where
        runRpc addr aSocket aMsg = do
          response <- call methods (fromStrict aMsg)
          sendAllTo aSocket (toStrict $ fromMaybe "" response) addr
            where
              methods = [createTx , createNTx, createUnlimTx, genNewKey, getKeys, balanceReq, sendMsgBroadcast, sendMsgTo, loadMsg ]

              createTx = toMethod "new_tx" f (Required "x" :+: ())
                where
                  f :: Trans -> RpcResult IO Transaction
                  f tx = do
                    mTx <- liftIO $ sendTrans tx ch aInfoCh
                    case mTx of
                      Just tx -> liftIO $ return tx
                      _       -> throwError $ rpcError 400 "You don't own that public key"

              createNTx = toMethod "gen_n_tx" f (Required "x" :+: ())
                where
                  f :: Int -> RpcResult IO ()
                  f num = liftIO $ generateNTransactions num ch aInfoCh

              createUnlimTx = toMethod "gen_unlim_tx" f ()
                where
                  f :: RpcResult IO ()
                  f = liftIO $ generateTransactionsForever ch aInfoCh

              genNewKey = toMethod "gen_new_key" f ()
                where
                  f :: RpcResult IO PubKey
                  f = liftIO $ getNewKey ch aInfoCh

              getKeys = toMethod "get_keys" f ()
                where
                  f :: RpcResult IO [PubKey]
                  f = liftIO $ getPublicKeys

              balanceReq = toMethod "get_balance" f (Required "x" :+: ())
                where
                  f :: PubKey -> RpcResult IO Amount
                  f key = liftIO $ getBalance key aInfoCh

              sendMsgBroadcast = toMethod "send_message_broadcast" f (Required "x" :+: ())
                where
                  f :: String -> RpcResult IO ()
                  f m = liftIO $ sendMessageBroadcast m ch

              sendMsgTo = toMethod "send_message_to" f (Required "x" :+: ())
                where
                  f :: MsgTo -> RpcResult IO ()
                  f m = liftIO $ sendMessageTo m ch

              loadMsg = toMethod "load_messages" f ()
                where
                  f :: RpcResult IO [MsgTo]
                  f = liftIO $ loadMessages ch


