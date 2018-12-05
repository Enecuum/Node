{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE DeriveAnyClass             #-}

module Enecuum.TestData.Nodes.Scenario5 where

import Enecuum.Prelude

import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L


-- Scenario 5: Permanent connection Ping-Pong

newtype Ping = Ping { ping :: Int }
  deriving (Show, Generic, ToJSON, FromJSON)

newtype Pong = Pong { pong :: Int }
  deriving (Show, Generic, ToJSON, FromJSON)

pongServerPort, pingClientPort :: D.PortNumber
pongServerPort = 2000
pingClientPort = 2001

pongServerAddress, pingClientAddress :: D.Address
pongServerAddress = D.Address "0.0.1.4" pongServerPort
pingClientAddress = D.Address "0.0.1.5" pingClientPort

pingPongThreshold :: Int
pingPongThreshold = 3

pingHandle :: D.StateVar Int -> Ping -> D.Connection D.Tcp -> L.NodeL ()
pingHandle countVar (Ping i) conn = do
    L.logInfo $ "Ping handle received: " +|| Ping i ||+ ". Sending " +|| Pong i ||+ "."
    void $ L.send conn $ Pong i
    when (i >= pingPongThreshold) $ L.close conn
    L.atomically $ L.writeVar countVar i

pongHandle :: D.StateVar Int -> Pong -> D.Connection D.Tcp -> L.NodeL ()
pongHandle countVar (Pong i) conn = do
    L.logInfo $ "Pong handle received: " +|| Pong i ||+ ". Sending " +|| Ping (i + 1) ||+ "."
    void $ L.send conn $ Ping $ i + 1
    when (i + 1 >= pingPongThreshold) $ L.close conn
    L.atomically $ L.writeVar countVar $ i + 1


pongServingNode :: L.NodeDefinitionL ()
pongServingNode = do
    countVar <- L.scenario $ L.atomically $ L.newVar 0

    L.servingMsg pongServerPort $ L.handler $ pingHandle countVar

    L.scenario $ L.atomically $ do
        pings <- L.readVar countVar
        when (pings < pingPongThreshold) L.retry

    L.stopServing pongServerPort

pingSendingClientNode :: L.NodeDefinitionL ()
pingSendingClientNode = L.scenario $ do
    countVar <- L.atomically $ L.newVar 0

    mCon <- L.open D.Tcp pongServerAddress $ L.handler $ pongHandle countVar
    whenJust mCon $ \conn -> do
        void $ L.send conn $ Ping 0

        L.atomically $ do
            pongs <- L.readVar countVar
            when (pongs < pingPongThreshold) L.retry

        L.close conn

