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
    , _logVar        :: D.StateVar [Text]
    }

makeFieldsNoPrefix ''GraphNodeData

-- stateLog :: GraphNodeData -> Text -> L.StateL ()
stateLog nodeData msg = L.modifyVar (nodeData ^. logVar) (msg:)

-- writeLog :: GraphNodeData -> Free L.NodeF ()
writeLog nodeData = do
    tmpLog <- L.atomically $ do
        tmpLog <- L.readVar (nodeData ^. logVar)
        L.writeVar (nodeData ^. logVar) []
        pure tmpLog
    forM_ (reverse tmpLog) L.logInfo

-- | Get kBlock by Hash
getKBlock :: GraphNodeData -> StringHash -> L.StateL (Maybe D.KBlock)
getKBlock nodeData hash = do
    (res, tmpLog) <- L.withGraph nodeData $ do
        maybeKBlock <- L.getNode hash
        case maybeKBlock of
            Just (D.HNode _ _ (D.fromContent -> D.KBlockContent kBlock) _ _)
                -> pure $ (Just kBlock,  "kBlock found by hash")
            _ -> pure (Nothing, "")
    stateLog nodeData tmpLog
    pure res

-- Get Top kBlock
getTopKeyBlock :: GraphNodeData -> L.StateL D.KBlock
getTopKeyBlock nodeData = do
    stateLog nodeData "Getting of top keyblock"
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
        []          -> do
            stateLog nodeData "Pending is empty"
            pure False
        kBlock:newPending | kBlockIsNext kBlock topKBlock -> do
            L.writeVar (nodeData ^. kBlockPending) newPending
            stateLog nodeData "Moving KBlock from pending to graph."
            addKBlock nodeData kBlock
        _ -> do
            stateLog nodeData "Is impossible move block from pending to graph."
            pure False


kBlockIsNext :: D.KBlock -> D.KBlock -> Bool
kBlockIsNext kBlock topKBlock = 
    kBlock ^. Lens.number   == topKBlock ^. Lens.number + 1 &&
    kBlock ^. Lens.prevHash == toHash topKBlock

-- | Add new key block to pending. 
addBlockToPending :: GraphNodeData -> D.KBlock -> L.StateL Bool
addBlockToPending nodeData kBlock = do
    stateLog nodeData "Addition of block to pending"
    L.modifyVar
        (nodeData ^. kBlockPending)
        (\pending -> sortOn (^. Lens.number) $ kBlock:pending)
    pure True

-- | Add key block to graph        
addKBlock :: GraphNodeData -> D.KBlock -> L.StateL Bool
addKBlock nodeData kBlock = do
    stateLog nodeData "Addition kblock to end of chain"
    let kBlock' = D.KBlockContent kBlock
    ref <- L.readVar (nodeData ^. curNode)
    L.withGraph nodeData $ do
        L.newNode kBlock'
        L.newLink ref kBlock'
    -- change of curNode.
    L.writeVar (nodeData ^. curNode) $ toHash kBlock'
    return True

-- | Add microblock to graph
addMBlock :: GraphNodeData -> D.Microblock -> L.StateL Bool
addMBlock nodeData mblock@(D.Microblock hash _) = do
    kblock <- getKBlock nodeData hash
    stateLog nodeData $ "Addition of mblock to graph. In graph is needed kblock? " <> show (isJust kblock)
    when (isJust kblock) $ L.withGraph nodeData $ do
        L.newNode (D.MBlockContent mblock)
        L.newLink hash (D.MBlockContent mblock)
    pure $ isJust kblock

-- | Accept kBlock 
acceptKBlock :: GraphNodeData -> D.KBlock -> L.NodeL (Either Text SuccesMsg)
acceptKBlock nodeData kBlock = do
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
    writeLog nodeData
    if res
        then pure $ Right SuccesMsg
        else pure $ Left "Error of kblock accepting"


-- | Accept mBlock 
acceptMBlock :: GraphNodeData -> D.Microblock -> L.NodeL (Either Text SuccesMsg)
acceptMBlock nodeData mBlock = do
    writeLog nodeData
    res <- L.atomically (addMBlock nodeData mBlock)
    if res
        then pure $ Right SuccesMsg
        else pure $ Left "Error of mblock accepting"

getLastKBlock :: GraphNodeData -> GetLastKBlock -> L.NodeL D.KBlock
getLastKBlock nodeData _ = L.atomically $ getTopKeyBlock nodeData

-- | Initialization of graph node
graphNodeInitialization :: L.NodeL GraphNodeData
graphNodeInitialization = do
    g <- L.newGraph
    L.evalGraphIO g $ L.newNode $ D.KBlockContent D.genesisKBlock
    L.atomically $ GraphNodeData g
        <$> L.newVar []
        <*> L.newVar D.genesisHash
        <*> L.newVar []

-- | Start of graph node
graphNode :: L.NodeDefinitionL ()
graphNode = do
    L.nodeTag "graphNode"
    nodeData <- L.initialization graphNodeInitialization

    L.serving 2001 $ do
        L.methodE $ acceptKBlock nodeData
        L.methodE $ acceptMBlock nodeData
        L.method  $ getLastKBlock nodeData