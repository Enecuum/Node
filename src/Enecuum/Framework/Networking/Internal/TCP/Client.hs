{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Enecuum.Framework.Networking.Internal.TCP.Client (
    ConnectInfo(..),
    runClient,
    PortNumber(..),
    HostAddress,
  ) where

import           Prelude
import           Enecuum.Legacy.Service.Network.Base
import           Network.Socket
import           Control.Exception


class (Show a) => Hosts a where
    openConnect :: a -> PortNumber -> IO Socket

-- | Run a TCP client.
runClient :: Hosts a => a -> PortNumber -> (Socket -> IO ()) -> IO ()
runClient aHostAddress port aPlainHandler = withSocketsDo $ do
    connection <- openConnect aHostAddress port)
    finally (aPlainHandler connection) (close connection)

instance Hosts HostAddress where
    openConnect address = openConnect (showHostAddress address)

instance Hosts String where
    openConnect address port = do
        connectInfo <- head <$> getAddrInfo Nothing (Just address) (Just $ show port)
        sock <- socket (addrFamily connectInfo) Stream defaultProtocol
        connect sock $ addrAddress connectInfo
        pure sock
