{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Enecuum.Legacy.Service.Network.TCP.Client (
    ConnectInfo(..),
    runClient,
    PortNumber(..),
    HostAddress,
    openConnect,
    closeConnect
  ) where

import           Data.Text                           (pack)
import           Enecuum.Legacy.Service.Network.Base
import           Enecuum.Prelude
import           Network.Socket


class (Show a) => Hosts a where
    openConnect :: a -> PortNumber -> IO ClientHandle

closeConnect :: ClientHandle -> IO ()
closeConnect = close . clientSocket

-- | Run a TCP client.
runClient :: Hosts a => a -> PortNumber -> (ClientHandle -> IO ()) -> IO ()
runClient aHostAddress aPort aPlainHandler = withSocketsDo $ do
    aHandle <- (try $ openConnect aHostAddress aPort) >>= \case
      Right h                     -> return h
      Left (err :: SomeException) -> error $ pack $ "TCP socket connection exception on " ++ show aHostAddress ++ ":" ++ show aPort ++ " :" ++ show err
    (try $ aPlainHandler aHandle) >>= \case
      Right _                     -> closeConnect aHandle
      Left (err :: SomeException) -> putStrLn $ "TCP socket exception on " ++ show aHostAddress ++ ":" ++ show aPort ++ " :" ++ show err

instance Hosts HostAddress where
    openConnect aHostAdress = openConnect (showHostAddress aHostAdress)

instance Hosts String where
    openConnect aHostAdress aPort = do
        aServerAddr <- head <$> getAddrInfo
            Nothing
            (Just aHostAdress)
            (Just $ show aPort)
        aSocket <- socket (addrFamily aServerAddr) Stream defaultProtocol
        connect aSocket $ addrAddress aServerAddr
        return $ ClientHandle aSocket (addrAddress aServerAddr)
