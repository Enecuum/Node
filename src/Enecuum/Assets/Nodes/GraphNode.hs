{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Assets.Nodes.GraphNode where

import           Enecuum.Prelude

import qualified Data.Aeson                    as A
import qualified Data.Map                      as Map
import           Control.Lens                  (makeFieldsNoPrefix)

import           Enecuum.Config                (Config)
import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import qualified Enecuum.Blockchain.Lens       as Lens
import qualified Enecuum.Framework.Lens        as Lens
import qualified Enecuum.Core.Lens             as Lens
import qualified Data.Text as Text
import           Enecuum.Framework.Language.Extra (HasGraph)

import qualified Enecuum.Blockchain.Domain.Graph as TG
import           Enecuum.Assets.Nodes.RPC
import           Enecuum.Assets.Nodes.Address
import           Enecuum.Assets.Nodes.Types

data GraphNodeData = GraphNodeData
    { _graph :: TG.GraphVar
    , _curNode :: D.StateVar D.StringHash
    }

makeFieldsNoPrefix ''GraphNodeData

newtype AcceptKeyBlockRequest  = AcceptKeyBlockRequest { kBlock :: D.KBlock }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AcceptKeyBlockResponse = AcceptKeyBlockResponse { accepted :: Bool }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


validateKBlock :: GraphNodeData -> D.KBlock -> L.StateL Bool
validateKBlock nodeData kBlock = do
    topNodeHash <- L.readVar $ nodeData ^. curNode

    prevBlockNumber <- L.withGraph nodeData $ do
        mbTopNode <- L.getNode topNodeHash
        case mbTopNode of
            Nothing -> error "Not impossible happened: hash is absent in graph."
            Just _ -> error "not implemented"
    
    pure $ (1 + prevBlockNumber) == (kBlock ^. Lens.number)

addBlock :: GraphNodeData -> D.KBlock -> L.StateL Bool
addBlock nodeData kBlock = error "not implemented."

acceptKBlock :: GraphNodeData -> AcceptKeyBlockRequest -> L.NodeL AcceptKeyBlockResponse
acceptKBlock nodeData (AcceptKeyBlockRequest kBlock) = do

    result <- L.atomically $ do

        validated <- validateKBlock nodeData kBlock
        case validated of
            True -> addBlock nodeData kBlock
            False -> pure False

    pure $ AcceptKeyBlockResponse result



graphNodeInitialization :: TG.GraphVar -> L.NodeL GraphNodeData
graphNodeInitialization g = do
    baseNode <- L.evalGraphIO g $ L.getNode TG.genesisHash >>= \case
        Nothing       -> error "Genesis node not found. Graph is not ready."
        Just baseNode -> pure baseNode
    baseNodeVar <- L.atomically $ L.newVar $ baseNode ^. Lens.hash
    pure $ GraphNodeData g baseNodeVar


graphNode :: TG.GraphVar -> L.NodeDefinitionL ()
graphNode g = do
    L.nodeTag "graphNode"
    nodeData <- L.initialization $ graphNodeInitialization g

    L.servingRpc 2001 $ do
        L.method $ acceptKBlock nodeData
