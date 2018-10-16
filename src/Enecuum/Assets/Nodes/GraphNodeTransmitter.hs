{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf             #-}

module Enecuum.Assets.Nodes.GraphNodeTransmitter (graphNodeTransmitter) where

import           Enecuum.Prelude
import Data.Map (fromList, lookup, insert, Map(..), elems, keys, empty)
import           Control.Lens                  (makeFieldsNoPrefix)


import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import qualified Enecuum.Blockchain.Lens       as Lens
import qualified Enecuum.Core.Lens             as Lens
import           Data.HGraph.StringHashable

import           Enecuum.Framework.Language.Extra (HasGraph, HasStatus, NodeStatus (..))

import qualified Enecuum.Blockchain.Domain.Graph as TG
import           Enecuum.Assets.Nodes.Messages
import           Enecuum.Assets.Nodes.Address

import qualified Enecuum.Framework.LogState as Log

data GraphNodeData = GraphNodeData
    { _blockchain    :: D.BlockchainData
    , _logVar        :: D.StateVar [Text]
    , _status        :: D.StateVar NodeStatus
    }

makeFieldsNoPrefix ''GraphNodeData

-- | Get kBlock by Hash
getKBlock :: D.StateVar [Text] -> D.BlockchainData -> StringHash -> L.StateL (Maybe D.KBlock)
getKBlock logV bData hash = do
    (res, mbMsg) <- L.withGraph bData $ do
        maybeKBlock <- L.getNode hash
        case maybeKBlock of
            Just (D.HNode _ _ (D.fromContent -> D.KBlockContent kBlock) _ _) -> pure $ (Just kBlock, Nothing)
            _ -> pure (Nothing, Just $ "KBlock not found by hash: " <> show hash)
    whenJust mbMsg $ Log.stateLog logV
    pure res

-- Get Top kBlock
getTopKeyBlock :: D.StateVar [Text] -> D.BlockchainData -> L.StateL D.KBlock
getTopKeyBlock logV bData = do
    topNodeHash    <- L.readVar $ bData ^. Lens.curNode
    Just topKBlock <- getKBlock logV bData topNodeHash
    pure topKBlock

-- | Move one block from pending to graph if it is possibly and remove it from pending.
--   Return true if function had effect.
moveKBlockToGraph :: D.StateVar [Text] -> D.BlockchainData -> L.StateL Bool
moveKBlockToGraph logV bData = do
    topKBlock <- getTopKeyBlock logV bData
    pending   <- L.readVar (bData ^. Lens.kBlockPending)
    case pending of
        kBlock : newPending | kBlockIsNext kBlock topKBlock -> do
            L.writeVar (bData ^. Lens.kBlockPending) newPending
            Log.stateLog logV "Moving KBlock from pending to graph."
            addKBlock logV bData kBlock
        _ -> pure False


kBlockIsNext :: D.KBlock -> D.KBlock -> Bool
kBlockIsNext kBlock topKBlock =
    kBlock ^. Lens.number == topKBlock ^. Lens.number + 1 && kBlock ^. Lens.prevHash == toHash
        (D.KBlockContent topKBlock)

-- | Add new key block to pending.
addBlockToPending :: D.StateVar [Text] -> D.BlockchainData -> D.KBlock -> L.StateL Bool
addBlockToPending logV bData kBlock = do
    Log.stateLog logV "Adding KBlock to pending"
    L.modifyVar (bData ^. Lens.kBlockPending) (\pending -> sortOn (^. Lens.number) $ kBlock : pending)
    pure True

-- | Add key block to graph
addKBlock :: D.StateVar [Text] -> D.BlockchainData -> D.KBlock -> L.StateL Bool
addKBlock logV bData kBlock = do
    Log.stateLog logV "Adding KBlock to the graph."
    let kBlock' = D.KBlockContent kBlock
    ref <- L.readVar (bData ^. Lens.curNode)
    L.withGraph bData $ do
        L.newNode kBlock'
        L.newLink ref kBlock'
    -- change of curNode.
    L.writeVar (bData ^. Lens.curNode) $ toHash kBlock'
    pure True

-- | Add microblock to graph
addMBlock :: D.StateVar [Text] -> D.BlockchainData -> D.Microblock -> L.StateL Bool
addMBlock logV bData mblock@(D.Microblock hash _ _ _) = do
    kblock <- getKBlock logV bData hash

    unless (isJust kblock) $ Log.stateLog logV $ "Can't add MBlock to the graph: KBlock not found (" +|| hash ||+ ")."

    when (isJust kblock) $ do
        Log.stateLog logV $ "Adding MBlock to the graph for KBlock (" +|| hash ||+ ")."
        calculateLedger logV bData mblock
        L.withGraph bData $ do
            L.newNode (D.MBlockContent mblock)
            L.newLink hash (D.MBlockContent mblock)
    pure $ isJust kblock

calculateLedger :: D.StateVar [Text] -> D.BlockchainData -> D.Microblock -> Free L.StateF ()
calculateLedger logV bData mblock = do
    forM_ (mblock ^. Lens.transactions) $ \tx -> do
        ledgerW <- L.readVar $ bData ^. Lens.ledger
        -- stateLog nodeData $ "Current Ledger " +|| ledgerW ||+ "."
        let owner          = tx ^. Lens.owner
            receiver       = tx ^. Lens.receiver
            amount         = tx ^. Lens.amount
            currentBalance = lookup owner ledgerW

        when (owner == receiver) $ Log.stateLog logV $ "Tx rejected (same owner and receiver): " +|| owner ||+ "."

        when (owner /= receiver) $ do
            ownerBalance <- case currentBalance of
                    Nothing           -> (Log.stateLog logV $ "Can't find wallet in ledger: " +|| owner ||+ ".") >> pure 100
                    Just ownerBalance -> pure ownerBalance 
            if ownerBalance >= amount 
            then do
-- stateLog nodeData $ "Before tx owner " +|| owner ||+ " has balance: " +|| balance ||+ "."
                let receiverBalance = fromMaybe 0 $ lookup receiver ledgerW
                -- stateLog nodeData $ "Before tx receiver " +|| receiver ||+ " has balance: " +|| receiverBalance ||+ "."
                let
                    newLedger = insert owner
                                       (ownerBalance - amount)
                                       (insert receiver (receiverBalance + amount) ledgerW)
                L.writeVar (bData ^. Lens.ledger) newLedger
                Log.stateLog logV
                    $   "Tx accepted: from [" +|| owner ||+ "] to [" +|| receiver ||+ "], amount: " +|| amount ||+ ". ["
                    +|| owner ||+ "]: " +|| ownerBalance - amount ||+ ", [" +|| receiver ||+ "]: " +|| receiverBalance + amount ||+ ""
-- stateLog nodeData $ "New Ledger " +|| newLedger ||+ "."
            else
                Log.stateLog logV
                $   "Tx rejected (negative balance): [" +|| owner ||+ "] -> [" +|| receiver ||+ "], amount: " +|| amount ||+ "."

-- | Accept kBlock
acceptKBlock :: GraphNodeData -> D.KBlock -> D.Connection D.Udp -> L.NodeL ()
acceptKBlock nodeData kBlock _ = do
    L.logInfo $ "\nAccepting KBlock (" +|| toHash kBlock ||+ "): " +|| kBlock ||+ "."
    let logV = nodeData ^. logVar
        bData = nodeData ^. blockchain
    res <- L.atomically $ do
        topKBlock <- getTopKeyBlock logV bData
        if
            | kBlockIsNext kBlock topKBlock -> do
                void $ addKBlock logV bData kBlock
                let loop = whenM (moveKBlockToGraph logV bData) loop
                loop
                pure True
            | kBlock ^. Lens.number > topKBlock ^. Lens.number + 1 -> addBlockToPending logV bData kBlock
            | otherwise -> pure False
    Log.writeLog logV


-- | Accept mBlock
acceptMBlock :: GraphNodeData -> D.Microblock -> D.Connection D.Udp -> L.NodeL ()
acceptMBlock nodeData mBlock _ = do
    L.logInfo "Accepting MBlock."
    let logV = nodeData ^. logVar
        bData = nodeData ^. blockchain
    void $ L.atomically (addMBlock logV bData mBlock)
    Log.writeLog logV

getLastKBlock :: GraphNodeData ->  GetLastKBlock -> L.NodeL D.KBlock
getLastKBlock nodeData _ = do
    let logV = nodeData ^. logVar
        bData = nodeData ^. blockchain
    -- L.logInfo "Top KBlock requested."
    kBlock <- L.atomically $ getTopKeyBlock logV bData
    -- L.logInfo $ "Top KBlock (" +|| toHash kBlock ||+ "): " +|| kBlock ||+ "."
    pure kBlock

getBalance :: GraphNodeData -> GetWalletBalance -> L.NodeL (Either Text WalletBalanceMsg)
getBalance nodeData (GetWalletBalance wallet) = do
    L.logInfo $ "Requested balance for wallet " +|| wallet ||+ "."
    let bData = nodeData ^. blockchain
    curLedger <- L.atomically $ L.readVar $ bData ^. Lens.ledger
    let maybeBalance = lookup wallet curLedger
    case maybeBalance of
        Just balance -> pure $ Right $ WalletBalanceMsg wallet balance
        _            -> pure $ Left "Wallet does not exist in graph."

acceptChainLength :: GraphNodeData -> GetChainLengthRequest -> L.NodeL GetChainLengthResponse
acceptChainLength nodeData GetChainLengthRequest = do
    let logV = nodeData ^. logVar
        bData = nodeData ^. blockchain
    L.logInfo "Answering chain length"
    topKBlock <- L.atomically $ getTopKeyBlock logV bData        
    Log.writeLog logV
    pure $ GetChainLengthResponse $ topKBlock ^. Lens.number
    
findBlocksByNumber :: D.StateVar [Text] -> D.BlockchainData -> Integer -> D.KBlock -> L.StateL [D.KBlock]
findBlocksByNumber logV bData num prev = 
    let cNum = prev ^. Lens.number in
    if 
        | cNum < num -> pure []
        | cNum == num -> pure [prev]
        | cNum > num -> do
            maybeNext <- getKBlock logV bData (prev ^. Lens.prevHash)
            case maybeNext of
                Nothing -> error "Broken chain"
                Just next -> (:) prev <$> findBlocksByNumber logV bData num next
            

acceptChainFromTo :: GraphNodeData -> GetChainFromToRequest -> L.NodeL (Either Text GetChainFromToResponse)
acceptChainFromTo nodeData (GetChainFromToRequest from to) = do
    L.logInfo $ "Answering chain from " +|| show from ||+ " to " +|| show to
    let logV = nodeData ^. logVar
        bData = nodeData ^. blockchain
    if from > to
        then pure $ Left "From is grater than to"
        else do
            kBlockList <- L.atomically $ do
                topKBlock <- getTopKeyBlock logV bData
                chain <- findBlocksByNumber logV bData from topKBlock
                pure $ drop (fromEnum $ (topKBlock ^. Lens.number) - to) chain
            Log.writeLog logV    
            pure $ Right $ GetChainFromToResponse (reverse kBlockList)

acceptMBlockForKBlocks :: GraphNodeData -> GetMBlocksForKBlockRequest -> L.NodeL (Either Text GetMBlocksForKBlockResponse)
acceptMBlockForKBlocks nodeData (GetMBlocksForKBlockRequest hash) = do
    L.logInfo $ "Answering microblocks for kBlock " +|| show hash
    let logV = nodeData ^. logVar
        bData = nodeData ^. blockchain
    mBlockList <- L.atomically $ L.withGraph bData $ do 
        node <- L.getNode hash
        case node of
            Nothing -> pure Nothing
            Just (D.HNode _ _ _ links _) -> do
                aMBlocks                       <- forM (Data.Map.keys links) $ \aNRef -> do
                    Just (D.HNode _ _ (D.fromContent -> block) _ _) <- L.getNode aNRef
                    case block of
                        D.MBlockContent mBlock -> pure $ Just mBlock
                        _               -> pure Nothing
                pure $ Just $ catMaybes aMBlocks
    Log.writeLog logV
    case mBlockList of
        Nothing -> pure $ Left "KBlock does't exist"  
        Just blockList -> pure $ Right $ GetMBlocksForKBlockResponse blockList

-- | Initialization of graph node
graphNodeInitialization :: L.NodeDefinitionL GraphNodeData
graphNodeInitialization = L.scenario $ do
    g <- L.newGraph
    L.evalGraphIO g $ L.newNode $ D.KBlockContent D.genesisKBlock
    L.logInfo $ "Genesis block (" +|| D.genesisHash ||+ "): " +|| D.genesisKBlock ||+ "."
    L.atomically
        $  GraphNodeData <$> (D.BlockchainData g
        <$> L.newVar []
        <*> L.newVar D.genesisHash
        <*> L.newVar Data.Map.empty)
        <*> L.newVar []
        <*> L.newVar NodeActing
        
-- | Start of graph node
graphNodeTransmitter :: L.NodeDefinitionL ()
graphNodeTransmitter = do
    L.nodeTag "graphNodeTransmitter"
    nodeData <- graphNodeInitialization

    L.serving D.Udp graphNodeTransmitterUdpPort $ do
        L.handler $ acceptMBlock nodeData
        L.handler $ acceptKBlock nodeData

    L.serving D.Rpc graphNodeTransmitterRpcPort $ do
        L.method  $ getLastKBlock nodeData
        L.methodE $ getBalance nodeData
        L.method  $ acceptChainLength nodeData
        L.methodE $ acceptChainFromTo nodeData
        L.methodE $ acceptMBlockForKBlocks nodeData

    L.std $ L.stdHandler $ L.stopNodeHandler nodeData
    L.awaitNodeFinished nodeData
