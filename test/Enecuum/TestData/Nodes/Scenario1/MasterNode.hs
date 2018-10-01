
module Enecuum.TestData.Nodes.Scenario1.MasterNode where

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

-- Scenario 1: master node can interact with boot node.

simpleBootNodeDiscovery :: L.NetworkL D.Address
simpleBootNodeDiscovery = pure bootNodeAddr

masterNodeInitialization :: L.NodeL (Either Text D.NodeID)
masterNodeInitialization = do
  addr <- L.evalNetworking $ L.evalNetwork simpleBootNodeDiscovery
  GetHashIDResponse eHashID <- L.makeRpcRequestUnsafe addr GetHashIDRequest
  pure $ Right (D.NodeID eHashID)

masterNode :: L.NodeDefinitionL ()
masterNode = do
  L.nodeTag masterNodeTag
  nodeId <- D.withSuccess $ L.initialization masterNodeInitialization
  L.logInfo $ "Master node got id: " +|| nodeId ||+ "."
  L.servingRpc 2000 $ do
      L.method acceptHello1
      L.method acceptHello2
