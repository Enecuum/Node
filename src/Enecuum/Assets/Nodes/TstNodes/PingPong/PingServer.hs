{-# OPTIONS_GHC -fno-warn-orphans   #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Enecuum.Assets.Nodes.TstNodes.PingPong.PingServer where

import           Enecuum.Config
import qualified Enecuum.Domain                              as D
import qualified Enecuum.Language                            as L
import           Enecuum.Prelude
import           Enecuum.Assets.Nodes.TstNodes.PingPong.Messages

data PingServerData = PingServerData
    { _pingsCount :: D.StateVar Int
    }

makeFieldsNoPrefix ''PingServerData

data instance NodeConfig PingServerNode = PingServerNode
    { _stopOnPing :: Int
    }
    deriving (Show, Generic)

instance Node PingServerNode where
    data NodeScenario PingServerNode = PingServer
        deriving (Show, Generic)
    getNodeScript _ = pingServerNode
    getNodeTag _ = PingServerNode

instance ToJSON   (NodeScenario PingServerNode) where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeScenario PingServerNode) where parseJSON = J.genericParseJSON nodeConfigJsonOptions

acceptPing :: PingServerNodeData -> Ping -> connection -> L.NodeL ()
acceptPing nodeData (Ping clientName) conn = do
    pings <- L.atomically $ do
        pings <- L.readVar $ nodeData ^. pingsCount
        let newPings = pings + 1
        L.writeVar (nodeData ^. pingsCount) newPings
        pure newPings
    L.send conn (Pong pings)
    L.close conn
    L.logInfo $ "Ping #" +|| pings ||+ " accepted from " <> clientName <> "."

pingServerNode :: NodeConfig PingServerNode -> L.NodeDefinitionL ()
pingServerNode cfg = do
    nodeData <- initializePingServerNode

    L.serving D.Udp 3000 $ do
        L.method $ acceptPing nodeData

    L.atomically $ do
        pings <- readVar $ nodeData ^. pingsCount
        when (pings < _stopOnPing cfg) L.retry

initializePingServerNode ::  NodeConfig PingServerNode -> L.NodeL PingServerNodeData
initializePingServerNode cfg = do
    pingsCount <- L.newVarIO 0
    pure PingServerNodeData
        { _pingsCount = pingsCount
        }
