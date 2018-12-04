{-# OPTIONS_GHC -fno-warn-orphans   #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Assets.Nodes.TstNodes.PingPong.PingServer where

import qualified Data.Aeson                                      as A
import           Enecuum.Assets.Nodes.TstNodes.PingPong.Messages
import           Enecuum.Config
import qualified Enecuum.Domain                                  as D
import qualified Enecuum.Language                                as L
import           Enecuum.Prelude

data PingServerNode = PingServerNode
    deriving (Show, Generic)

data instance NodeConfig PingServerNode = PingServerNodeConfig
    { _stopOnPing  :: Int
    , _servingPort :: D.PortNumber
    }
    deriving (Show, Generic)

instance Node PingServerNode where
    data NodeScenario PingServerNode = PingServer
        deriving (Show, Generic)
    getNodeScript _ = pingServerNode
    getNodeTag _ = PingServerNode

instance ToJSON   PingServerNode                where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON PingServerNode                where parseJSON = A.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeConfig PingServerNode)   where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig PingServerNode)   where parseJSON = A.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeScenario PingServerNode) where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeScenario PingServerNode) where parseJSON = A.genericParseJSON nodeConfigJsonOptions

-- Handling Ping messages.
acceptPing
    :: D.StateVar D.NodeStatus
    -> D.StateVar Int
    -> Int
    -> Ping
    -> D.Connection D.Udp
    -> L.NodeL ()
acceptPing status pingsCount threshold (Ping clientName) conn = do
    pings <- L.atomically $ do
        L.modifyVar pingsCount (+1)
        L.readVar pingsCount

    let done = pings + 1 >= threshold
    when done $ do
        L.close conn
        L.writeVarIO status D.NodeFinished
        L.logInfo $ "Pings threshold reached: " +|| threshold ||+ ". Finishing."

    unless done $ do
        L.send conn (Pong pings)
        L.logInfo $ "Ping #" +|| pings ||+ " accepted from " +|| clientName ||+ "."

-- Ping server definition node.
pingServerNode :: NodeConfig PingServerNode -> L.NodeDefinitionL ()
pingServerNode cfg = do
    let threshold = _stopOnPing cfg
    let port = _servingPort cfg

    pingsCount <- L.newVarIO 0
    status     <- L.newVarIO D.NodeActing

    -- Starting a separate process for serving on UDP port.
    L.serving D.Udp port $
        L.handler $ acceptPing status pingsCount threshold

    L.awaitNodeFinished' status
