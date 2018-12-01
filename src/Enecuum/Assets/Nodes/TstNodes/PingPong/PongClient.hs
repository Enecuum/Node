{-# OPTIONS_GHC -fno-warn-orphans   #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Assets.Nodes.TstNodes.PingPong.PongClient where

import qualified Data.Aeson                                      as A
import           Enecuum.Assets.Nodes.TstNodes.PingPong.Messages
import           Enecuum.Config
import qualified Enecuum.Domain                                  as D
import           Enecuum.Framework.Language.Extra                (HasStatus)
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
    getNodeScript _ = pongClientNode'
    getNodeTag _ = PongClientNode

instance ToJSON   PongClientNode                where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON PongClientNode                where parseJSON = A.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeConfig PongClientNode)   where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeConfig PongClientNode)   where parseJSON = A.genericParseJSON nodeConfigJsonOptions
instance ToJSON   (NodeScenario PongClientNode) where toJSON    = A.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeScenario PongClientNode) where parseJSON = A.genericParseJSON nodeConfigJsonOptions

acceptPong :: Pong -> connection -> L.NodeL ()
acceptPong (Pong pingsCount) _ =
    L.logInfo $ "Pong accepted from server. Pings count: " <> show pingsCount

pingSending' :: NodeConfig PongClientNode -> D.Connection D.Udp -> L.NodeL ()
pingSending' cfg conn = do
    L.delay $ _pingDelay cfg
    L.logInfo "Sending Ping to the server."
    eSent <- L.send conn (Ping $ _clientName cfg)
    case eSent of
        Right () -> pingSending' cfg conn
        Left _   -> do
            L.logInfo "Server is gone."
            L.close conn


pongClientNode' :: NodeConfig PongClientNode -> L.NodeDefinitionL ()
pongClientNode' cfg = do

    mbConn <- L.open D.Udp (_pingServerAddress cfg) $
        L.handler acceptPong

    case mbConn of
        Nothing -> L.logError "Ping Server not found"
        Just conn -> do
            L.process (pingSending' cfg conn)
            L.awaitNodeForever

pingSending :: Text -> D.Connection D.Udp -> L.NodeL ()
pingSending clientName conn = do
    L.delay 1000000
    L.logInfo "Sending Ping to the server."
    eSent <- L.send conn (Ping clientName)
    when (isLeft eSent) $ L.close conn
    when (isRight eSent) $ pingSending clientName conn

pongClientNode :: Text -> D.Address -> L.NodeDefinitionL ()
pongClientNode clientName serverAddress = do

    mbConn <- L.open D.Udp serverAddress $
        L.handler acceptPong

    when (isNothing mbConn) $ L.logError "Ping Server not found"
    whenJust mbConn $ \conn -> do
        L.process $ pingSending clientName conn
        L.awaitNodeForever
