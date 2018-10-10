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

-- | Accept kBlock
acceptKBlock :: GraphNodeData -> D.KBlock -> L.NodeL (Either Text SuccessMsg)
acceptKBlock nodeData kBlock = do
    L.logInfo $ "\nAccepting KBlock (" +|| toHash kBlock ||+ "): " +|| kBlock ||+ "."
    res <- L.atomically $ do
        topKBlock <- getTopKeyBlock nodeData
        if
            | kBlockIsNext kBlock topKBlock -> do
                void $ addKBlock nodeData kBlock
                let loop = whenM (moveKBlockToGraph nodeData) loop
                loop
                pure True
            | kBlock ^. Lens.number > topKBlock ^. Lens.number + 1 -> addBlockToPending nodeData kBlock
            | otherwise -> pure False
    writeLog nodeData
    if res then pure $ Right SuccessMsg else pure $ Left "Error of kblock accepting"


-- | Accept mBlock
acceptMBlock :: GraphNodeData -> D.Microblock -> L.NodeL (Either Text SuccessMsg)
acceptMBlock nodeData mBlock = do
    L.logInfo "Accepting MBlock."
    res <- L.atomically (addMBlock nodeData mBlock)
    writeLog nodeData
    if res then pure $ Right SuccessMsg else pure $ Left "Error of mblock accepting"

getLastKBlock :: GraphNodeData -> GetLastKBlock -> L.NodeL D.KBlock
getLastKBlock nodeData _ = do
    -- L.logInfo "Top KBlock requested."
    kBlock <- L.atomically $ getTopKeyBlock nodeData
    -- L.logInfo $ "Top KBlock (" +|| toHash kBlock ||+ "): " +|| kBlock ||+ "."
    pure kBlock

getBalance :: GraphNodeData -> GetWalletBalance -> L.NodeL (Either Text WalletBalanceMsg)
getBalance nodeData (GetWalletBalance wallet) = do
    L.logInfo $ "Requested balance for wallet " +|| wallet ||+ "."
    curLedger <- L.atomically $ L.readVar $ nodeData ^. ledger
    let maybeBalance = lookup wallet curLedger
    case maybeBalance of
        Just balance -> pure $ Right $ WalletBalanceMsg wallet balance
        _            -> pure $ Left "Wallet does not exist in graph."

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
graphNode :: L.NodeDefinitionL ()
graphNode = do
    L.nodeTag "graphNode"
    nodeData <- graphNodeInitialization

    L.serving graphNodeRpcPort $ do
        L.methodE $ acceptKBlock nodeData
        L.methodE $ acceptMBlock nodeData
        L.method $ getLastKBlock nodeData
        L.methodE $ getBalance nodeData

    L.std $ L.stdHandler $ L.setNodeFinished nodeData
    L.nodeFinishPending nodeData
    L.stopServing graphNodeRpcPort
