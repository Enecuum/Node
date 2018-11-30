{-# OPTIONS_GHC -fno-warn-orphans   #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Enecuum.Assets.Nodes.TstNodes.PingPong.PongClient where

import           Enecuum.Config
import qualified Enecuum.Domain                              as D
import           Enecuum.Framework.Language.Extra            (HasStatus)
import qualified Enecuum.Language                            as L
import           Enecuum.Prelude
import           Enecuum.Assets.Nodes.TstNodes.PingPong.Messages

data PongClientData = PongClientData
    { _pingsCount :: D.StateVar Int
    }

makeFieldsNoPrefix ''PongClientData

data instance NodeConfig PongClientNode = PongClientNode
    { _clientName :: Text
    }
    deriving (Show, Generic)

instance Node PongClientNode where
    data NodeScenario PongClientNode = PongClient
        deriving (Show, Generic)
    getNodeScript _ = PongClientNode
    getNodeTag _ = PongClientNode

instance ToJSON   (NodeScenario PongClientNode) where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeScenario PongClientNode) where parseJSON = J.genericParseJSON nodeConfigJsonOptions

acceptPing :: PongClientNodeData -> Ping -> connection -> L.NodeL ()
acceptPing nodeData (Ping clientName) conn = do
    pings <- L.atomically $ do
        pings <- L.readVar $ nodeData ^. pingsCount
        let newPings = pings + 1
        L.writeVar (nodeData ^. pingsCount) newPings
        pure newPings
    L.send conn (Pong pings)
    L.close conn
    L.logInfo $ "Ping #" +|| pings ||+ " accepted from " <> clientName <> "."

PongClientNode :: NodeConfig PongClientNode -> L.NodeDefinitionL ()
PongClientNode cfg = do
    nodeData <- initializePongClientNode

    L.serving D.Udp 3000 $ do
        L.method $ acceptPing nodeData

    L.atomically $ do
        pings <- readVar $ nodeData ^. pingsCount
        when (pings < _stopOnPing cfg) L.retry

initializePongClientNode ::  NodeConfig PongClientNode -> L.NodeL PongClientNodeData
initializePongClientNode cfg = do
    pingsCount <- L.newVarIO 0
    pure PongClientNodeData
        { _pingsCount = pingsCount
        }
