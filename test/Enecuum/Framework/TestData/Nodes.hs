{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Enecuum.Framework.TestData.Nodes where

import Enecuum.Prelude

import qualified Data.Aeson                    as A
import qualified Data.ByteString.Lazy          as BS
import           Eff.SafeIO                    (safeIO)

import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import qualified Enecuum.Framework.Lens        as Lens
import qualified Enecuum.API                   as API

bootNodeTag = "bootNode"
masterNodeTag = "masterNode"

localNetwork = error "localNetwork"

-- Just dummy types
data GetNeighboursRequest = GetNeighboursRequest
  deriving (Generic, ToJSON, FromJSON)

data GetHashIDRequest = GetHashIDRequest
newtype GetHashIDResponse = GetHashIDResponse Text
  deriving (Generic, Newtype)

instance D.RpcMethod () GetHashIDRequest GetHashIDResponse where
  toRpcRequest _ _ = D.RpcRequest
  fromRpcResponse _ _ = Just $ GetHashIDResponse "1"

data AcceptConnectionRequest = AcceptConnectionRequest
  deriving (Generic, ToJSON, FromJSON)
data HelloRequest1 = HelloRequest1 String
  deriving (Generic, ToJSON, FromJSON)
data HelloRequest2 = HelloRequest2 String
  deriving (Generic, ToJSON, FromJSON)

simpleBootNodeDiscovery :: Eff L.NetworkModel D.NodeConfig
simpleBootNodeDiscovery = do
  mbBootNodeCfg <- fmap unpack <$> (L.multicastRequest localNetwork 10.0 $ API.FindNodeByTagRequest bootNodeTag)
  case mbBootNodeCfg of
    Nothing          -> pure $ D.NodeConfig $ D.ConnectionConfig     -- Dummy
    Just bootNodeCfg -> pure bootNodeCfg

acceptHello1 :: HelloRequest1 -> Eff L.NodeModel ()
acceptHello1 (HelloRequest1 msg) = error $ "Accepting HelloRequest1: " ++ msg

acceptHello2 :: HelloRequest2 -> Eff L.NodeModel ()
acceptHello2 (HelloRequest2 msg) = error $ "Accepting HelloRequest2: " ++ msg

bootNode :: (Member L.NodeDefinitionL effs) => Eff effs D.NodeDef
bootNode = do
  _         <- L.nodeTag bootNodeTag
  nodeID    <- L.initialization $ pure $ D.NodeID "abc"
  serverDef <- L.serving $ L.serve @HelloRequest1 acceptHello1
  pure $ D.NodeDef nodeID serverDef

-- TODO: with NodeModel, evalNework not needed.
-- TODO: make TCP Sockets / WebSockets stuff more correct
masterNodeInitialization :: Eff L.NodeModel (Either Text D.NodeID)
masterNodeInitialization = do
  bootNodeCfg <- L.evalNetwork simpleBootNodeDiscovery
  eHashID     <- fmap unpack <$> L.withConnection (bootNodeCfg ^. Lens.connectionConfig) GetHashIDRequest
  pure $ eHashID >>= Right . D.NodeID

masterNode :: (Member L.NodeDefinitionL effs) => D.Config -> Eff effs D.NodeDef
masterNode _ = do
  _         <- L.nodeTag masterNodeTag
  -- TODO: handle the error correctly.
  nodeID    <- D.withSuccess $ L.initialization masterNodeInitialization
  serverDef <- L.serving
    $ L.serve @HelloRequest1 acceptHello1
    . L.serve @HelloRequest2 acceptHello2
  pure $ D.NodeDef nodeID serverDef
