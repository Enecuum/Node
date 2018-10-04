{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Assets.Nodes.GraphNode where

import           Enecuum.Prelude

import           Control.Lens                  (makeFieldsNoPrefix)


import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import qualified Enecuum.Blockchain.Lens       as Lens
import qualified Enecuum.Core.Lens             as Lens

import           Enecuum.Framework.Language.Extra (HasGraph)

import qualified Enecuum.Blockchain.Domain.Graph as TG
import           Enecuum.Assets.Nodes.Messages

data GraphNodeData = GraphNodeData
    { _graph :: TG.GraphVar
    , _curNode :: D.StateVar D.StringHash
    }

makeFieldsNoPrefix ''GraphNodeData


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
            True  -> addBlock nodeData kBlock
            False -> pure False

    pure $ AcceptKeyBlockResponse result



graphNodeInitialization :: L.NodeL GraphNodeData
graphNodeInitialization = do
    g <- L.newGraph
    baseNode <- L.evalGraphIO g $ L.getNode TG.genesisHash >>= \case
        Nothing       -> error "Genesis node not found. Graph is not ready."
        Just baseNode -> pure baseNode
    baseNodeVar <- L.atomically $ L.newVar $ baseNode ^. Lens.hash
    pure $ GraphNodeData g baseNodeVar


graphNode :: L.NodeDefinitionL ()
graphNode = do
    L.nodeTag "graphNode"
    nodeData <- L.initialization $ graphNodeInitialization

    L.serving 2001 $
        L.method $ acceptKBlock nodeData
