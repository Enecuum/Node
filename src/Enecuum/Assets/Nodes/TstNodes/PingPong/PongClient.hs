{-# OPTIONS_GHC -fno-warn-orphans   #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Assets.Nodes.TstNodes.PingPong.PongClient where

import qualified Data.Aeson                                      as A
import           Enecuum.Assets.Nodes.TstNodes.PingPong.Messages
import           Enecuum.Config
import qualified Enecuum.Domain                                  as D
import qualified Enecuum.Language                                as L
import           Enecuum.Prelude

data PongClientNode = PongClientNode
    deriving (Show, Generic)

data instance NodeConfig PongClientNode = PongClientNodeConfig
    { _clientName        :: Text
    , _pingDelay         :: Int
    , _pingServerAddress :: D.Address
    }
    deriving (Show, Generic)

instance Node PongClientNode where
    data NodeScenario PongClientNode = PongClient
        deriving (Show, Generic)
    getNodeScript _ = pongClientNode
    getNodeTag _ = PongClientNode

instance ToJSON   PongClientNode                where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON PongClientNode                where parseJSON = A.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeConfig PongClientNode)   where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig PongClientNode)   where parseJSON = A.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeScenario PongClientNode) where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeScenario PongClientNode) where parseJSON = A.genericParseJSON nodeConfigJsonOptions

-- Accepting pong responses from the server.
acceptPong :: Pong -> connection -> L.NodeL ()
acceptPong (Pong pingsCount) _ =
    L.logInfo $ "Pong accepted from server. Pings count: " <> show pingsCount

-- Sending pings to the server.
pingSending :: D.StateVar D.NodeStatus -> NodeConfig PongClientNode -> D.Connection D.Udp -> L.NodeL ()
pingSending status cfg conn = do
    L.delay $ _pingDelay cfg
    L.logInfo "Sending Ping to the server."
    eSent <- L.send conn (Ping $ _clientName cfg)
    case eSent of
        Right () -> pingSending status cfg conn
        Left _   -> do
            L.logInfo "Server is gone."
            L.close conn
            L.writeVarIO status D.NodeFinished

-- Pong client definition node.
pongClientNode :: NodeConfig PongClientNode -> L.NodeDefinitionL ()
pongClientNode cfg = do
    status <- L.newVarIO D.NodeActing

    -- Connecting to the server.
    mbConn <- L.open D.Udp (_pingServerAddress cfg) $
        L.handler acceptPong

    case mbConn of
        Nothing -> L.logError "Ping Server not found"
        Just conn -> do
            -- Forking separate process of periodical pings.
            L.process (pingSending status cfg conn)
            -- Waiting when the node is finished.
            L.awaitNodeFinished' status
