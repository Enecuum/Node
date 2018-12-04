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
    getNodeScript _ = pingServerNode'
    getNodeTag _ = PingServerNode

instance ToJSON   PingServerNode                where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON PingServerNode                where parseJSON = A.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeConfig PingServerNode)   where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig PingServerNode)   where parseJSON = A.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeScenario PingServerNode) where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeScenario PingServerNode) where parseJSON = A.genericParseJSON nodeConfigJsonOptions

acceptPing :: D.StateVar Int -> Ping -> D.Connection D.Udp -> L.NodeL ()
acceptPing pingsCount (Ping clientName) conn = do
    pings <- L.atomically $ do
        L.modifyVar pingsCount (+1)
        L.readVar pingsCount
    L.send conn (Pong pings)
    L.logInfo $ "Ping #" +|| pings ||+ " accepted from " +|| clientName ||+ "."

pingServerNode :: Int -> D.PortNumber -> L.NodeDefinitionL ()
pingServerNode threshold port = do
    pingsCount <- L.newVarIO 0

    L.serving D.Udp port $
        L.handler $ acceptPing pingsCount

    L.atomically $ do
        pings <- L.readVar pingsCount
        when (pings < threshold) L.retry

pingServerNode' :: NodeConfig PingServerNode -> L.NodeDefinitionL ()
pingServerNode' cfg = pingServerNode (_stopOnPing cfg) (_servingPort cfg)
