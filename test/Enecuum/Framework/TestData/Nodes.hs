{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Framework.TestData.Nodes where

import Enecuum.Prelude

import qualified Data.Aeson                    as A

import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import qualified Enecuum.Framework.Lens        as Lens

import           Enecuum.Framework.TestData.RPC

bootNodeAddr, masterNode1Addr :: D.NodeAddress
bootNodeAddr = "boot node addr"
masterNode1Addr = "master node 1 addr"

bootNodeTag, masterNodeTag :: D.NodeTag
bootNodeTag = "bootNode"
masterNodeTag = "masterNode"

-- | Boot node discovery sample scenario.
-- Currently, does nothing but returns the default boot node address.
simpleBootNodeDiscovery :: Eff L.NetworkModel D.NodeAddress
simpleBootNodeDiscovery = pure bootNodeAddr

-- RPC handlers.

acceptHello1 :: HelloRequest1 -> Eff L.NodeModel HelloResponse1
acceptHello1 (HelloRequest1 msg) = pure $ HelloResponse1 $ "Hello, dear. " +| msg |+ ""

acceptHello2 :: HelloRequest2 -> Eff L.NodeModel HelloResponse2
acceptHello2 (HelloRequest2 msg) = pure $ HelloResponse2 $ "Hello, dear2. " +| msg |+ ""

acceptGetHashId :: GetHashIDRequest -> Eff L.NodeModel GetHashIDResponse
acceptGetHashId GetHashIDRequest = pure $ GetHashIDResponse "1"

acceptValidationRequest :: ValidationRequest -> Eff L.NodeModel ValidationResponse
acceptValidationRequest ValidRequest   = pure $ ValidationResponse "correct"
acceptValidationRequest InvalidRequest = pure $ ValidationResponse "invalid"

-- Boot node scenario

bootNode :: (Member L.NodeDefinitionL effs) => Eff effs ()
bootNode = do
  L.nodeTag bootNodeTag
  L.initialization $ pure $ D.NodeID "abc"
  L.serving
    $ L.serve @HelloRequest1 @HelloResponse1 acceptHello1
    . L.serve @GetHashIDRequest @GetHashIDResponse acceptGetHashId
    . L.serve @ValidationRequest @ValidationResponse acceptValidationRequest

-- Master node scenario

masterNodeInitialization :: Eff L.NodeModel (Either Text D.NodeID)
masterNodeInitialization = do
  addr     <- L.evalNetwork simpleBootNodeDiscovery
  eHashID  <- fmap unpack <$> L.withConnection (D.ConnectionConfig addr) GetHashIDRequest
  validRes <- fmap unpack <$> L.withConnection (D.ConnectionConfig addr) ValidRequest
  L.logInfo $ "For the valid request recieved " +|| validRes ||+ "."
  invalidRes <- fmap unpack <$> L.withConnection (D.ConnectionConfig addr) InvalidRequest
  L.logInfo $ "For the invalid request recieved " +|| invalidRes ||+ "."
  pure $ eHashID >>= Right . D.NodeID

masterNode :: Eff L.NodeDefinitionModel ()
masterNode = do
  L.nodeTag masterNodeTag
  nodeId <- D.withSuccess $ L.initialization masterNodeInitialization
  L.logInfo $ "Master node got id: " +|| nodeId ||+ "."
  L.serving
    $ L.serve @HelloRequest1 @HelloResponse1 acceptHello1
    . L.serve @HelloRequest2 @HelloResponse2 acceptHello2
