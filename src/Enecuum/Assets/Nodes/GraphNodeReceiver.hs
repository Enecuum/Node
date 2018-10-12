{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf             #-}

module Enecuum.Assets.Nodes.GraphNodeReceiver (graphNodeReceiver) where

import           Enecuum.Prelude
import Data.Map (fromList, lookup, insert, Map(..))
import           Control.Lens                  (makeFieldsNoPrefix)


import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import qualified Enecuum.Blockchain.Lens       as Lens
import qualified Enecuum.Core.Lens             as Lens
import           Data.HGraph.StringHashable

import           Enecuum.Framework.Language.Extra (HasGraph, HasFinished)

import qualified Enecuum.Blockchain.Domain.Graph as TG
import           Enecuum.Assets.Nodes.Messages
import           Enecuum.Assets.Nodes.Address

data GraphNodeData = GraphNodeData
    { _graph         :: TG.GraphVar
    , _kBlockPending :: D.StateVar [D.KBlock]
    , _curNode       :: D.StateVar D.StringHash
    , _logVar        :: D.StateVar [Text]
    , _ledger        :: D.StateVar (Map Integer Integer)
    , _finished      :: D.StateVar Bool
    }

makeFieldsNoPrefix ''GraphNodeData

stateLog :: GraphNodeData -> Text -> L.StateL ()
stateLog nodeData msg = L.modifyVar (nodeData ^. logVar) (msg :)

writeLog :: GraphNodeData -> L.NodeL ()
writeLog nodeData = do
    tmpLog <- L.atomically $ do
        tmpLog <- L.readVar (nodeData ^. logVar)
        L.writeVar (nodeData ^. logVar) []
        pure tmpLog
    forM_ (reverse tmpLog) L.logInfo

-- | Get kBlock by Hash
getKBlock :: GraphNodeData -> StringHash -> L.StateL (Maybe D.KBlock)
getKBlock nodeData hash = do
    (res, mbMsg) <- L.withGraph nodeData $ do
        maybeKBlock <- L.getNode hash
        case maybeKBlock of
            Just (D.HNode _ _ (D.fromContent -> D.KBlockContent kBlock) _ _) -> pure $ (Just kBlock, Nothing)
            _ -> pure (Nothing, Just $ "KBlock not found by hash: " <> show hash)
    whenJust mbMsg $ stateLog nodeData
    pure res

-- Get Top kBlock
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
        [] -> do
            -- stateLog nodeData "Pending is empty"
            pure False
        kBlock : newPending | kBlockIsNext kBlock topKBlock -> do
            L.writeVar (nodeData ^. kBlockPending) newPending
            stateLog   nodeData                    "Moving KBlock from pending to graph."
            addKBlock  nodeData                    kBlock
        _ -> do
            stateLog nodeData "It's impossible to move block from pending to graph."
            pure False


kBlockIsNext :: D.KBlock -> D.KBlock -> Bool
kBlockIsNext kBlock topKBlock =
    kBlock ^. Lens.number == topKBlock ^. Lens.number + 1 && kBlock ^. Lens.prevHash == toHash
        (D.KBlockContent topKBlock)

-- | Add new key block to pending.
addBlockToPending :: GraphNodeData -> D.KBlock -> L.StateL Bool
addBlockToPending nodeData kBlock = do
    stateLog    nodeData                    "Adding KBlock to pending"
    L.modifyVar (nodeData ^. kBlockPending) (\pending -> sortOn (^. Lens.number) $ kBlock : pending)
    pure True

-- | Add key block to graph
addKBlock :: GraphNodeData -> D.KBlock -> L.StateL Bool
addKBlock nodeData kBlock = do
    stateLog nodeData "Adding KBlock to the graph."
    let kBlock' = D.KBlockContent kBlock
    ref <- L.readVar (nodeData ^. curNode)
    L.withGraph nodeData $ do
        L.newNode kBlock'
        L.newLink ref kBlock'
    -- change of curNode.
    L.writeVar (nodeData ^. curNode) $ toHash kBlock'
    pure True

-- | Add microblock to graph
addMBlock :: GraphNodeData -> D.Microblock -> L.StateL Bool
addMBlock nodeData mblock@(D.Microblock hash _) = do
    kblock <- getKBlock nodeData hash

    unless (isJust kblock) $ stateLog nodeData $ "Can't add MBlock to the graph: KBlock not found (" +|| hash ||+ ")."

    when (isJust kblock) $ do
        stateLog nodeData $ "Adding MBlock to the graph for KBlock (" +|| hash ||+ ")."
        calculateLedger nodeData mblock
        L.withGraph nodeData $ do
            L.newNode (D.MBlockContent mblock)
            L.newLink hash (D.MBlockContent mblock)
    pure $ isJust kblock

calculateLedger :: GraphNodeData -> D.Microblock -> Free L.StateF ()
calculateLedger nodeData mblock = do
    forM_ (mblock ^. Lens.transactions) $ \tx -> do
        ledgerW <- L.readVar $ nodeData ^. ledger
        -- stateLog nodeData $ "Current Ledger " +|| ledgerW ||+ "."
        let owner          = tx ^. Lens.owner
            receiver       = tx ^. Lens.receiver
            amount         = tx ^. Lens.amount
            currentBalance = lookup owner ledgerW

        when (owner == receiver) $ stateLog nodeData $ "Tx rejected (same owner and receiver): " +|| owner ||+ "."

        when (owner /= receiver) $ do
            case currentBalance of
                Nothing           -> stateLog nodeData $ "Can't find wallet in ledger: " +|| owner ||+ "."
                Just ownerBalance -> if ownerBalance >= amount
                    then do
-- stateLog nodeData $ "Before tx owner " +|| owner ||+ " has balance: " +|| balance ||+ "."
                        let receiverBalance = fromMaybe 0 $ lookup receiver ledgerW
                        -- stateLog nodeData $ "Before tx receiver " +|| receiver ||+ " has balance: " +|| receiverBalance ||+ "."
                        let
                            newLedger = insert owner
                                               (ownerBalance - amount)
                                               (insert receiver (receiverBalance + amount) ledgerW)
                        L.writeVar (nodeData ^. ledger) newLedger
                        stateLog nodeData
                            $   "Tx accepted: from [" +|| owner ||+ "] to [" +|| receiver ||+ "], amount: " +|| amount ||+ ". ["
                            +|| owner ||+ "]: " +|| ownerBalance - amount ||+ ", [" +|| receiver ||+ "]: " +|| receiverBalance + amount ||+ ""
-- stateLog nodeData $ "New Ledger " +|| newLedger ||+ "."
                    else
                        stateLog nodeData
                        $   "Tx rejected (negative balance): [" +|| owner ||+ "] -> [" +|| receiver ||+ "], amount: " +|| amount ||+ "."



getBalance :: GraphNodeData -> GetWalletBalance -> L.NodeL (Either Text WalletBalanceMsg)
getBalance nodeData (GetWalletBalance wallet) = do
    L.logInfo $ "Requested balance for wallet " +|| wallet ||+ "."
    curLedger <- L.atomically $ L.readVar $ nodeData ^. ledger
    let maybeBalance = lookup wallet curLedger
    case maybeBalance of
        Just balance -> pure $ Right $ WalletBalanceMsg wallet balance
        _            -> pure $ Left "Wallet does not exist in graph."

graphSynchro :: GraphNodeData -> D.Address -> L.NodeL ()
graphSynchro nodeData address = do
    L.logInfo $ "Requests chain length."
    GetChainLengthResponse otherLength <- L.makeRpcRequestUnsafe address GetChainLengthRequest
    L.logInfo $ "GraphNodeReceiver has Chain length: " +|| otherLength ||+ "."

    L.logInfo $ "GraphNodeReceiver: update chain if it's bigger."
    curChainLength <- L.atomically $ do
        topKBlock <- getTopKeyBlock nodeData
        pure $ topKBlock ^. Lens.number
    
    L.logInfo $ "Current chain length " +|| show curChainLength
    

    when (curChainLength < otherLength) $ do
        GetChainFromToResponse chainTail <- L.makeRpcRequestUnsafe address (GetChainFromToRequest (curChainLength + 1) otherLength)
        L.logInfo $ "Chain tail received from " +|| show (curChainLength + 1) ||+ " to " +|| show otherLength ||+ " : " +|| show chainTail
        L.atomically $ forM_ chainTail (addKBlock nodeData)
        for_ chainTail $ \kBlock -> do
            let hash = toHash kBlock
            GetMBlocksForKBlockResponse mBlocks <- L.makeRpcRequestUnsafe address (GetMBlocksForKBlockRequest hash)
            L.logInfo $ "Mblocks received for kBlock " +|| show hash ||+ " : " +|| show mBlocks
            L.atomically $ forM_ mBlocks (addMBlock nodeData)
    L.logInfo $ "Graph sychro finished"

-- | Initialization of graph node
graphNodeInitialization :: L.NodeDefinitionL GraphNodeData
graphNodeInitialization = L.scenario $ do
    g <- L.newGraph
    let wallets = zip [1 .. 5] (repeat 100)
    L.evalGraphIO g $ L.newNode $ D.KBlockContent D.genesisKBlock
    L.logInfo $ "Genesis block (" +|| D.genesisHash ||+ "): " +|| D.genesisKBlock ||+ "."
    L.atomically
        $   GraphNodeData g
        <$> L.newVar []
        <*> L.newVar D.genesisHash
        <*> L.newVar []
        <*> L.newVar (fromList wallets)
        <*> L.newVar False

-- | Start of graph node
graphNodeReceiver :: L.NodeDefinitionL ()
graphNodeReceiver = do
    L.nodeTag "graphNodeReceiver"
    nodeData <- graphNodeInitialization

    L.scenario $ graphSynchro nodeData graphNodeTransmitterRpcAddress

    L.serving graphNodeReceiverRpcPort $ do
        L.methodE $ getBalance nodeData

    L.std $ L.stdHandler $ L.setNodeFinished nodeData
    L.nodeFinishPending nodeData
    L.stopServing graphNodeReceiverRpcPort
