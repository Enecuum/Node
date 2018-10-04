{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf             #-}

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
    { _graph         :: TG.GraphVar
    , _kBlockPending :: D.StateVar [D.KBlock]
    , _curNode       :: D.StateVar D.StringHash
    }

makeFieldsNoPrefix ''GraphNodeData


getKBlock :: GraphNodeData -> StringHash -> L.StateL (Maybe D.KBlock)
getKBlock nodeData hash = L.withGraph nodeData $ do
    maybeKBlock <- L.getNode hash
    case maybeKBlock of
        Just (D.HNode _ _ (D.fromContent -> D.KBlockContent kBlock) _ _)
            -> pure $ Just kBlock
        _ -> pure Nothing

getTopKeyBlock :: GraphNodeData -> L.StateL D.KBlock
getTopKeyBlock nodeData = do
    topNodeHash    <- L.readVar $ nodeData ^. curNode
    Just topKBlock <- getKBlock nodeData topNodeHash
    pure topKBlock

-- | Move one block from pending to graph if it is possibly and remove it from pending.
--   Return true if function had effect.
moveKBlockToGraph :: GraphNodeData -> L.StateL Bool
moveKBlockToGraph nodeData = do
    topKBlock <- getTopKeyBlock nodeData
    pending   <- L.readVar (nodeData ^. kBlockPending)
    case pending of
        []          -> pure False
        kBlock:newPending | kBlockIsNext kBlock topKBlock -> do
            L.writeVar (nodeData ^. kBlockPending) newPending
            addKBlock nodeData kBlock
        _ -> pure False

kBlockIsNext :: D.KBlock -> D.KBlock -> Bool
kBlockIsNext kBlock topKBlock = 
    kBlock ^. Lens.number   == topKBlock ^. Lens.number + 1 &&
    kBlock ^. Lens.prevHash == toHash topKBlock

-- | Add new key block too pending. 
addBlockToPending :: GraphNodeData -> D.KBlock -> L.StateL Bool
addBlockToPending nodeData kBlock = do
    L.modifyVar
        (nodeData ^. kBlockPending)
        (\pending -> sortOn (^. Lens.number) $ kBlock:pending)
    pure True

validateKBlock :: GraphNodeData -> D.KBlock -> L.StateL Bool
validateKBlock nodeData kBlock = do
    topKBlock <- getTopKeyBlock nodeData
    pure $ (1 + topKBlock ^. Lens.number == kBlock ^. Lens.number)
        && (kBlock ^. Lens.prevHash == toHash topKBlock)

validationKBlockForPending :: GraphNodeData -> D.KBlock -> L.StateL Bool
validationKBlockForPending nodeData kBlock = do
    topKBlock <- getTopKeyBlock nodeData
    pure $ topKBlock ^. Lens.number < (kBlock ^. Lens.number)

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
    kblock <- getKBlock nodeData hash
    when (isJust kblock) $ L.withGraph nodeData $ do
        L.newNode (D.MBlockContent mblock)
        L.newLink hash (D.MBlockContent mblock)
    pure $ isJust kblock

-- | Accept kBlock 
acceptKBlock :: GraphNodeData -> AcceptKeyBlockRequest -> L.NodeL AcceptKeyBlockResponse
acceptKBlock nodeData (AcceptKeyBlockRequest kBlock) = do
    res <- L.atomically $ do
        topKBlock <- getTopKeyBlock nodeData
        if  | kBlock ^. Lens.number >  topKBlock ^. Lens.number + 1 ->
                addBlockToPending nodeData kBlock
            
            | kBlockIsNext kBlock topKBlock -> do
                void $ addKBlock nodeData kBlock
                let loop = whenM (moveKBlockToGraph nodeData) loop
                loop
                pure True

            | otherwise -> pure False

    pure $ AcceptKeyBlockResponse res

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
    
    L.atomically $
        GraphNodeData g <$> L.newVar [] <*> L.newVar (baseNode ^. Lens.hash) 


graphNode :: L.NodeDefinitionL ()
graphNode = do
    L.nodeTag "graphNode"
    nodeData <- L.initialization graphNodeInitialization

    L.serving 2001 $ do
        L.method $ acceptKBlock nodeData
        L.method $ acceptMBlock nodeData