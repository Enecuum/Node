{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Enecuum.TestData.Nodes.Scenario5 where

import Enecuum.Prelude

import qualified Data.Aeson                    as A
import qualified Data.Map                      as Map
import qualified Data.Text                     as Text
import           Control.Lens                  (makeFieldsNoPrefix)

import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import qualified Enecuum.Blockchain.Lens       as Lens
import qualified Enecuum.Framework.Lens        as Lens
import qualified Enecuum.Core.Lens             as Lens
import           Enecuum.Language              (HasGraph)

import qualified Enecuum.Core.HGraph.Internal.Types as T

import           Enecuum.TestData.RPC
import qualified Enecuum.TestData.TestGraph as TG
import           Enecuum.TestData.Nodes.Address

-- Scenario 5: Permanent connection Ping-Pong

newtype Ping = Ping Int
  deriving (Show, Generic, ToJSON, FromJSON)

newtype Pong = Pong Int
  deriving (Show, Generic, ToJSON, FromJSON)


pongServerPort :: D.PortNumber
pongServerPort = 2000

pongServerAddress, pingClientAddress :: D.Address
pongServerAddress = D.Address "0.0.1.4" pongServerPort
pingClientAddress = D.Address "0.0.1.5" 2000

pingHandle :: D.StateVar Int -> Ping -> D.NetworkConnection -> L.NodeL ()
pingHandle countVar (Ping i) conn = do
    L.logInfo $ show $ Ping i
    L.evalNetworking $ L.send conn $ Pong i
    L.atomically $ L.writeVar countVar i

pongHandle :: D.StateVar Int -> Pong -> D.NetworkConnection -> L.NodeL ()
pongHandle countVar (Pong i) conn = do
    L.logInfo $ show $ Pong i
    L.evalNetworking $ L.send conn $ Ping $ i + 1
    L.atomically $ L.writeVar countVar $ i + 1


pongServingNode :: L.NodeDefinitionL ()
pongServingNode = do
    countVar <- L.scenario $ L.atomically $ L.newVar 0

    L.servingMsg pongServerPort $ L.handler $ pingHandle countVar

    L.scenario $ L.atomically $ do
        pings <- L.readVar countVar
        when (pings < 10) L.retry

    L.stopServing pongServerPort

pingSendingClientNode :: L.NodeDefinitionL ()
pingSendingClientNode = L.scenario $ do
    countVar <- L.atomically $ L.newVar 0

    conn <- L.open pongServerAddress $ L.handler $ pongHandle countVar

    L.logInfo "client node sends Pong 0."
    L.evalNetworking $ L.send conn $ Ping 0

    L.atomically $ do
        pongs <- L.readVar countVar
        when (pongs < 10) L.retry

    L.close conn

