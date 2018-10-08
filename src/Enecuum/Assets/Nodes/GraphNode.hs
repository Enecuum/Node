{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf             #-}

module Enecuum.Assets.Nodes.GraphNode where

import           Enecuum.Prelude
import Data.Map (fromList, lookup, insert, Map(..))
import           Control.Lens                  (makeFieldsNoPrefix)


import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import qualified Enecuum.Blockchain.Lens       as Lens
import qualified Enecuum.Core.Lens             as Lens
import           Data.HGraph.StringHashable

import           Enecuum.Framework.Language.Extra (HasGraph)

import qualified Enecuum.Blockchain.Domain.Graph as TG
import           Enecuum.Assets.Nodes.Messages
import           Enecuum.Assets.Nodes.Address

data GraphNodeData = GraphNodeData
    { _graph         :: TG.GraphVar
    , _kBlockPending :: D.StateVar [D.KBlock]
    , _curNode       :: D.StateVar D.StringHash
    , _logVar        :: D.StateVar [Text]
    , _ledger        :: D.StateVar (Map Integer Integer)
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
    stateLog nodeData $ "Hash of current top keyblock is " <> show topNodeHash
    Just topKBlock <- getKBlock nodeData topNodeHash
    stateLog nodeData $ "Current top keyblock is " <> show topKBlock
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
    kBlock ^. Lens.number   == topKBlock ^. Lens.number + 1 
    && kBlock ^. Lens.prevHash == toHash (D.KBlockContent topKBlock)

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
    when (isJust kblock) $ do
        calculateLedger nodeData mblock
        L.withGraph nodeData $ do
            L.newNode (D.MBlockContent mblock)
            L.newLink hash (D.MBlockContent mblock)
    pure $ isJust kblock

calculateLedger :: GraphNodeData -> D.Microblock -> Free L.StateF ()
calculateLedger nodeData mblock = do
    ledgerW <- L.readVar $ nodeData ^. ledger
    forM_ (mblock ^. Lens.transactions ) $ \ tx -> do
        let owner = tx ^. Lens.owner
            receiver = tx ^. Lens.receiver
            amount = tx ^. Lens.amount
            currentBalance = lookup owner ledgerW
        case currentBalance of
            Nothing -> stateLog nodeData $ "Can't find wallet in ledger: " +|| show owner
            Just balance -> if (balance >= amount)
                then do
                    let receiverBalance = fromMaybe 0 $ lookup receiver ledgerW
                        newLedger = insert owner (balance - amount) (insert receiver (receiverBalance + amount) ledgerW)
                    L.writeVar (nodeData ^. ledger ) newLedger
                    stateLog nodeData $ "Tx complete: from " +|| show owner +|| " , to " +|| show receiver ||+ " , amount " +|| show amount
                else
                    stateLog nodeData $ "Wallet trying to spend more than it have" +|| show owner

-- | Accept kBlock
acceptKBlock :: GraphNodeData -> D.KBlock -> L.NodeL (Either Text SuccessMsg)
acceptKBlock nodeData kBlock = do
    L.logInfo $ "Accepted kBlock" <> show kBlock
    res <- L.atomically $ do
        topKBlock <- getTopKeyBlock nodeData
        if  | kBlockIsNext kBlock topKBlock -> do
                void $ addKBlock nodeData kBlock
                let loop = whenM (moveKBlockToGraph nodeData) loop
                loop
                pure True

            | kBlock ^. Lens.number > topKBlock ^. Lens.number + 1 ->
                addBlockToPending nodeData kBlock

            | otherwise -> pure False
    writeLog nodeData
    if res
        then pure $ Right SuccessMsg
        else pure $ Left "Error of kblock accepting"


-- | Accept mBlock
acceptMBlock :: GraphNodeData -> D.Microblock -> L.NodeL (Either Text SuccessMsg)
acceptMBlock nodeData mBlock = do
    writeLog nodeData
    res <- L.atomically (addMBlock nodeData mBlock)
    if res
        then pure $ Right SuccessMsg
        else pure $ Left "Error of mblock accepting"

getLastKBlock :: GraphNodeData -> GetLastKBlock -> L.NodeL D.KBlock
getLastKBlock nodeData _ = do
    L.logInfo "Geting of last kblock."
    L.atomically $ getTopKeyBlock nodeData

getGraphNode :: GraphNodeData -> GetGraphNode -> L.NodeL (Either Text D.NodeContent)
getGraphNode nodeData (GetGraphNode hash) = do
    L.logInfo "Geting graph node by hash."
    L.atomically $ L.withGraph nodeData $ do
        maybeKBlock <- L.getNode hash
        case maybeKBlock of
            Just (D.HNode _ _ block _ _)
                -> pure $ Right $ D.fromContent block
            _   -> pure $ Left "Block not exist in graph."

-- | Initialization of graph node
graphNodeInitialization :: L.NodeL GraphNodeData
graphNodeInitialization = do
    g <- L.newGraph
    let wallets = zip [1..5] (repeat 100)
    L.evalGraphIO g $ L.newNode $ D.KBlockContent D.genesisKBlock
    L.atomically $ GraphNodeData g
        <$> L.newVar []
        <*> L.newVar D.genesisHash
        <*> L.newVar []
        <*> L.newVar (fromList wallets)

-- | Start of graph node
graphNode :: L.NodeDefinitionL ()
graphNode = do
    L.nodeTag "graphNode"
    nodeData <- L.initialization graphNodeInitialization

    L.serving graphNodeRpcPort $ do
        L.methodE $ acceptKBlock nodeData
        L.methodE $ acceptMBlock nodeData
        L.method $ getLastKBlock nodeData
        L.methodE $ getGraphNode nodeData