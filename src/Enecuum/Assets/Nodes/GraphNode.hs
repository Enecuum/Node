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
import           Data.HGraph.StringHashable

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

    topKBlock <- L.withGraph nodeData $ do
        mbTopNode <- L.getNode topNodeHash
        case mbTopNode of
            Nothing -> error "Not impossible happened: hash is absent in graph."
            Just (D.HNode _ _ (D.fromContent -> D.KBlockContent kBlock) _ _) -> pure kBlock
            Just _ -> error "Error: Microblock on top"

    pure ((1 + topKBlock ^. Lens.number) == (kBlock ^. Lens.number) &&
          (kBlock ^. Lens.prevHash == toHash topKBlock))


addKBlock :: GraphNodeData -> D.KBlock -> L.StateL Bool
addKBlock nodeData kBlock = do
    -- Add kblock to end of chain.
    let kBlock' = D.KBlockContent kBlock
    ref <- L.readVar (nodeData ^. curNode)
    L.withGraph nodeData $ do
        L.newNode kBlock'
        L.newLink ref kBlock'
    -- change of curNode.
    L.writeVar (nodeData ^. curNode) $ toHash kBlock'
    return True

addMBlock :: GraphNodeData -> D.Microblock -> L.StateL Bool
addMBlock nodeData mblock@(D.Microblock hash _) = do
    -- Add mblock to end of chain.
    let block = D.MBlockContent mblock
    L.withGraph nodeData $ do
        mKBlock <- L.getNode hash
        case mKBlock of
            Just (D.HNode _ _ (D.fromContent -> D.KBlockContent _) _ _) -> do
                L.newNode block
                L.newLink hash block
                pure True
            _ -> pure False

-- | Accept kBlock 
acceptKBlock :: GraphNodeData -> AcceptKeyBlockRequest -> L.NodeL AcceptKeyBlockResponse
acceptKBlock nodeData (AcceptKeyBlockRequest kBlock) =
    AcceptKeyBlockResponse <$> L.atomically
        (ifM (validateKBlock nodeData kBlock)
            (addKBlock nodeData kBlock)
            (pure False))

-- | Accept mBlock 
acceptMBlock :: GraphNodeData -> AcceptMicroblockRequest -> L.NodeL AcceptMicroblockResponse
acceptMBlock nodeData (AcceptMicroblockRequest mBlock) =
    AcceptMicroblockResponse <$> L.atomically (addMBlock nodeData mBlock)


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
    nodeData <- L.initialization graphNodeInitialization

    L.serving 2001 $ do
        L.method $ acceptKBlock nodeData
        L.method $ acceptMBlock nodeData