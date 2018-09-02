{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Enecuum.Framework.TestData.Nodes where

import           Data.Text                                ( Text )
import           Eff                                      ( Eff, Member )
import qualified Data.Aeson                    as A
import           Data.Aeson                               ( ToJSON
                                                          , FromJSON
                                                          )
import qualified Data.ByteString.Lazy          as BS
import           GHC.Generics                             ( Generic )
import           Control.Newtype.Generics                 ( Newtype
                                                          , O
                                                          , pack
                                                          , unpack
                                                          )
import           Eff.SafeIO                               (safeIO)

import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L

bootNodeTag = "bootNode"
masterNodeTag = "masterNode"

localNetwork = error "localNetwork"

request node req = error "request"
request' req node = flip request

-- Just dummy types
data GetNeighboursRequest = GetNeighboursRequest
  deriving (Generic, ToJSON, FromJSON)
data GetHashIDRequest = GetHashIDRequest
  deriving (Generic, ToJSON, FromJSON)
data AcceptConnectionRequest = AcceptConnectionRequest
  deriving (Generic, ToJSON, FromJSON)
data HelloRequest1 = HelloRequest1 String
  deriving (Generic, ToJSON, FromJSON)
data HelloRequest2 = HelloRequest2 String
  deriving (Generic, ToJSON, FromJSON)

simpleBootNodeDiscovery :: Eff L.NetworkModel D.NodeConfig
simpleBootNodeDiscovery = do
  mbBootNodeCfg <- fmap unpack <$> (L.multicastRequest localNetwork 10.0 $ D.FindNodeByTagRequest bootNodeTag)
  case mbBootNodeCfg of
    Nothing          -> pure D.NodeConfig     -- Dummy
    Just bootNodeCfg -> pure bootNodeCfg

acceptHello1 :: HelloRequest1 -> Eff L.NodeModel ()
acceptHello1 (HelloRequest1 msg) = error $ "Accepting HelloRequest1: " ++ msg

acceptHello2 :: HelloRequest2 -> Eff L.NodeModel ()
acceptHello2 (HelloRequest2 msg) = error $ "Accepting HelloRequest2: " ++ msg

bootNode ::(Member L.NodeDefinitionL effs) => Eff effs D.NodeDef
bootNode = do
  _         <- L.nodeTag bootNodeTag
  nodeID    <- L.initialization $ pure $ D.NodeID "abc"
  serverDef <- L.serving $ L.serve @HelloRequest1 acceptHello1
  pure $ D.NodeDef nodeID serverDef

-- TODO: with NodeModel, evalNework not needed.
-- TODO: make TCP Sockets / WebSockets stuff more correct
masterNodeInitialization :: Eff L.NodeModel D.NodeID
masterNodeInitialization = do
  bootNodeCfg <- L.evalNetwork simpleBootNodeDiscovery
  -- hashID      <- withConnection bootNodeCfg $ request' GetHashIDRequest
  let dummyHashID = ""
  pure $ D.NodeID dummyHashID

masterNode :: (Member L.NodeDefinitionL effs) => D.Config -> Eff effs D.NodeDef
masterNode _ = do
  _         <- L.nodeTag masterNodeTag
  nodeID    <- L.initialization masterNodeInitialization
  serverDef <- L.serving
    $ L.serve @HelloRequest1 acceptHello1
    . L.serve @HelloRequest2 acceptHello2
  pure $ D.NodeDef nodeID serverDef
