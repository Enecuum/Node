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

import Data.IP
import CLI.Common
import Node.Node.Types
import Service.Types.SerializeJSON ()
import Service.Types.PublicPrivateKeyPair
import Service.InfoMsg
import Service.Types
import Data.Text (pack)

serveRpc :: PortNumber -> [IPRange] -> Chan ManagerMiningMsgBase -> Chan InfoMsg -> IO ()
serveRpc portNum ips ch aInfoCh = runServer portNum $ \aSocket -> forever $ do
    (aMsg, addr) <- recvFrom aSocket (1024*100)

    if ipAccepted addr == False 
    then return ()
    else runRpc addr aSocket aMsg
      where
--        ipAccepted :: SockAddr -> Bool
        ipAccepted addr = do
          case fromSockAddr addr of
            Nothing      -> False
            Just (ip, _) -> True
{-                            foldl (\p ip_r -> case ip_r of
                         IPv4Range r -> isMatchedTo ip (IPv4 r)
                         IPv6Range r -> isMatchedTo ip (IPv6 r)
                         ) False ips
-}
        runRpc addr aSocket aMsg = do
          response <- call methods (fromStrict aMsg)
          sendAllTo aSocket (toStrict $ fromMaybe "" response) addr
            where
              handle f = do  
                    mTx <- liftIO $ f
                    case mTx of
                      Left e  -> throwError $ rpcError  400 $ pack $ show e
                      Right r -> liftIO $ return r


              methods = [createTx , createNTx, createUnlimTx, balanceReq, sendMsgBroadcast, sendMsgTo, loadMsg ]

              createTx = toMethod "new_tx" f (Required "x" :+: ())
                where
                  f :: Transaction -> RpcResult IO ()
                  f tx = handle $ sendTrans tx ch aInfoCh

              createNTx = toMethod "gen_n_tx" f (Required "x" :+: ())
                where
                  f :: Int -> RpcResult IO ()
                  f num = handle $ generateNTransactions num ch aInfoCh

              createUnlimTx = toMethod "gen_unlim_tx" f ()
                where
                  f :: RpcResult IO ()
                  f = handle $ generateTransactionsForever ch aInfoCh

              balanceReq = toMethod "get_balance" f (Required "x" :+: ())
                where
                  f :: PubKey -> RpcResult IO Amount
                  f key = handle $ getBalance key aInfoCh

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


