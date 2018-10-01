{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.TestData.Nodes.Scenario3.MasterNodeValidation where

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
import           Enecuum.TestData.Validation
import qualified Enecuum.TestData.TestGraph as TG
import           Enecuum.TestData.Nodes.Address

  -- Scenario 3: boot node can validate data  recieved from master node

masterNodeInitializeWithValidation :: L.NodeL (Either Text D.NodeID)
masterNodeInitializeWithValidation = do
  GetHashIDResponse eHashID <- L.makeRpcRequestUnsafe bootNodeAddr GetHashIDRequest
  validRes :: ValidationResponse <- L.makeRpcRequestUnsafe bootNodeAddr ValidRequest
  L.logInfo $ "For the valid request recieved " +|| validRes ||+ "."
  invalidRes :: ValidationResponse <- L.makeRpcRequestUnsafe bootNodeAddr InvalidRequest
  L.logInfo $ "For the invalid request recieved " +|| invalidRes ||+ "."
  pure $ Right (D.NodeID eHashID)

masterNodeValidation :: L.NodeDefinitionL ()
masterNodeValidation = do
  L.nodeTag masterNodeTag
  nodeId <- D.withSuccess $ L.initialization masterNodeInitializeWithValidation
  L.logInfo $ "Master node got id: " +|| nodeId ||+ "."
