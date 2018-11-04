{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE TemplateHaskell        #-}

module Enecuum.Assets.Nodes.GraphNode.Logic where

import           Enecuum.Prelude
import           Control.Lens                     (makeFieldsNoPrefix)
import           Data.HGraph.StringHashable
import qualified Data.Map                         as Map
import           System.FilePath                  ((</>))

import qualified Enecuum.Domain                   as D
import           Enecuum.Framework.Language.Extra (HasStatus, NodeStatus (..))
import qualified Enecuum.Framework.LogState       as Log
import qualified Enecuum.Language                 as L
import qualified Enecuum.Blockchain.Lens          as Lens
import qualified Enecuum.Blockchain.DB            as DB
import           Enecuum.Assets.Nodes.Messages
import           Enecuum.Assets.Nodes.GraphNode.Config
import qualified Enecuum.Assets.Nodes.CLens       as CLens
import qualified Enecuum.Assets.System.Directory  as L

data GraphNodeData = GraphNodeData
    { _blockchain         :: D.BlockchainData
    , _logVar             :: D.StateVar [Text]
    , _status             :: D.StateVar NodeStatus
    , _config             :: NodeConfig GraphNode
    , _db                 :: Maybe DB.DBModel
    , _dumpToDBSignal     :: D.StateVar Bool
    , _checkPendingSignal :: D.StateVar Bool
    }

makeFieldsNoPrefix ''GraphNodeData

transactionsToTransfer :: Int
transactionsToTransfer = 20

-- | DumpToDB command.
handleDumpToDB :: GraphNodeData -> DumpToDB -> L.NodeL (Either Text SuccessMsg)
handleDumpToDB nodeData _ = do
    L.writeVarIO (nodeData ^. dumpToDBSignal) True
    pure $ Right SuccessMsg

-- | Accept transaction
acceptTransaction :: GraphNodeData -> CreateTransaction -> L.NodeL (Either Text SuccessMsg)
acceptTransaction nodeData (CreateTransaction tx) = do
    L.logInfo $ "Got transaction " +| D.showTransaction tx "" |+ ""
    if L.verifyTransaction tx
        then do
            L.logInfo "\nTransaction is accepted"
            L.logInfo "\nAdd transaction to pending "
            let bData = nodeData ^. blockchain
            L.atomically $ L.modifyVar (bData ^. Lens.transactionPending) ( Map.insert (toHash tx ) tx)
            pure $ Right SuccessMsg
        else do
            L.logInfo "Transaction signature is not genuine"
            L.logInfo "Transaction is not accepted"
            pure $ Left "Transaction signature is not genuine. Transaction is not accepted."

-- | Accept kBlock
acceptKBlock' :: GraphNodeData -> D.KBlock -> L.NodeL ()
acceptKBlock' nodeData kBlock = do
    L.logInfo $ "Accepting KBlock (" +|| toHash kBlock ||+ "): " +|| kBlock ||+ "."
    let logV  = nodeData ^. logVar
    let bData = nodeData ^. blockchain
    kBlockAdded <- L.atomically $ L.addKBlock logV bData kBlock
    Log.writeLog logV
    L.writeVarIO (nodeData ^. checkPendingSignal) kBlockAdded

-- | Accept kBlock
acceptKBlock :: GraphNodeData -> D.KBlock -> D.Connection D.Tcp -> L.NodeL ()
acceptKBlock nodeData kBlock _ = acceptKBlock' nodeData kBlock

-- | Accept mBlock
acceptMBlock :: GraphNodeData -> D.Microblock -> D.Connection D.Tcp -> L.NodeL ()
acceptMBlock nodeData mBlock _ = do
    let res@(valid, _, _) = L.verifyMicroblockWithTx mBlock
    unless valid $ printInvalidSignatures res
    when valid $ do
        L.logInfo $ "Microblock " +|| toHash mBlock ||+ " is accepted."
        let logV  = nodeData ^. logVar
        let bData = nodeData ^. blockchain
        void $ L.atomically $ do
            void $ L.addMBlock logV bData mBlock
            let tx = mBlock ^. Lens.transactions
            let fun :: D.Transaction -> D.TransactionPending -> D.TransactionPending
                fun t = Map.delete (toHash t)
            forM_ tx (L.modifyVar (bData ^. Lens.transactionPending) . fun)
        Log.writeLog logV
    where
        printInvalidSignatures :: (Bool, Bool, [Bool]) -> L.NodeL ()
        printInvalidSignatures (valid, mBlockValid, txsValid) = do
            unless valid                 $ L.logInfo $ "Microblock is rejected: " +|| toHash mBlock ||+ "."
            unless mBlockValid           $ L.logInfo $ "Microblock has " +|| toHash mBlock ||+ " invalid signature."
            when (False `elem` txsValid) $ L.logInfo $ "Microblock " +|| toHash mBlock ||+ " transactions have invalid signature."

getKBlockPending :: GraphNodeData -> GetKBlockPending -> L.NodeL [D.KBlock]
getKBlockPending nodeData _ = do
    let bData = nodeData ^. blockchain
    L.readVarIO $ bData ^. Lens.kBlockPending
    -- kblocks <- L.readVar $ bData ^. Lens.kBlockPending
    -- L.modifyVar (bData ^. Lens.kBlockPending) (\_ -> [])
    -- pure kblocks


getTransactionPending :: GraphNodeData -> GetTransactionPending -> L.NodeL [D.Transaction]
getTransactionPending nodeData _ = do
    let bData = nodeData ^. blockchain
    L.atomically $ do
        trans <- L.readVar $ bData ^. Lens.transactionPending
        pure $ map snd $ take transactionsToTransfer $ Map.toList trans

getLastKBlock :: GraphNodeData -> GetLastKBlock -> L.NodeL D.KBlock
getLastKBlock nodeData _ = do
    let logV  = nodeData ^. logVar
    let bData = nodeData ^. blockchain
    -- L.logInfo "Top KBlock requested."
    L.atomically $ L.getTopKeyBlock logV bData
    -- L.logInfo $ "Top KBlock (" +|| toHash kBlock ||+ "): " +|| kBlock ||+ "."


getBalance :: GraphNodeData -> GetWalletBalance -> L.NodeL (Either Text WalletBalanceMsg)
getBalance nodeData (GetWalletBalance wallet) = do
    L.logInfo $ "Requested balance for wallet " +|| D.showPublicKey wallet ||+ "."
    let bData = nodeData ^. blockchain
    curLedger <- L.readVarIO $ bData ^. Lens.ledger
    case curLedger ^. at wallet of
        Just balance -> pure $ Right $ WalletBalanceMsg wallet balance
        _            -> pure $ Left $ "Wallet " +|| D.showPublicKey wallet ||+ " does not exist in graph."

getChainLength :: GraphNodeData -> GetChainLengthRequest -> L.NodeL GetChainLengthResponse
getChainLength nodeData GetChainLengthRequest = do
    let logV = nodeData ^. logVar
        bData = nodeData ^. blockchain
--    L.logInfo "Answering chain length"
    topKBlock <- L.atomically $ L.getTopKeyBlock logV bData
    Log.writeLog logV
    pure $ GetChainLengthResponse $ topKBlock ^. Lens.number


acceptChainFromTo :: GraphNodeData -> GetChainFromToRequest -> L.NodeL (Either Text GetChainFromToResponse)
acceptChainFromTo nodeData (GetChainFromToRequest from to) = do
    L.logInfo $ "Answering chain from " +|| (show from :: Text) ||+ " to " +|| show to
    let logV = nodeData ^. logVar
        bData = nodeData ^. blockchain
    if from > to
        then pure $ Left "From is greater than to"
        else do
            kBlockList <- L.atomically $ do
                topKBlock <- L.getTopKeyBlock logV bData
                chain <- L.findBlocksByNumber logV bData from topKBlock
                pure $ drop (fromEnum $ (topKBlock ^. Lens.number) - to) chain
            Log.writeLog logV
            pure $ Right $ GetChainFromToResponse (reverse kBlockList)

getMBlockForKBlocks :: GraphNodeData -> GetMBlocksForKBlockRequest -> L.NodeL (Either Text GetMBlocksForKBlockResponse)
getMBlockForKBlocks nodeData (GetMBlocksForKBlockRequest hash) = do
    L.logInfo $ "Get microblocks for kBlock " +|| show hash
    let logV = nodeData ^. logVar
        bData = nodeData ^. blockchain
    mBlockList <- L.atomically $ L.getMBlocksForKBlock logV bData hash
    Log.writeLog logV
    case mBlockList of
        Nothing        -> pure $ Left "KBlock doesn't exist"
        Just blockList -> pure $ Right $ GetMBlocksForKBlockResponse blockList


initializeDBModel :: NodeConfig GraphNode -> L.NodeL (Maybe DB.DBModel)
initializeDBModel nodeConfig = do

    parentDir <- if nodeConfig ^. CLens.useEnqHomeDir
        then L.getEnecuumDir
        else pure ""

    let dbModelPath = parentDir </> (nodeConfig ^. CLens.dbModelName)

    L.initDBModel dbModelPath $ nodeConfig ^. CLens.dbOptions

-- | Initialization of graph node
graphNodeInitialization :: NodeConfig GraphNode -> L.NodeDefinitionL (Either Text GraphNodeData)
graphNodeInitialization nodeConfig = L.scenario $ do

    let useDb       = nodeConfig ^. CLens.useDatabase
    let stopOnDbErr = nodeConfig ^. CLens.stopOnDatabaseError

    mbDBModel <- if useDb
        then initializeDBModel nodeConfig
        else pure Nothing

    g <- L.newGraph
    L.evalGraphIO g $ L.newNode $ D.KBlockContent D.genesisKBlock

    nodeData <- L.atomically
        $  GraphNodeData <$>
            ( D.BlockchainData g
                <$> L.newVar []
                <*> L.newVar Map.empty
                <*> L.newVar D.genesisHash
                <*> L.newVar Map.empty
            )
        <*> L.newVar []
        <*> L.newVar NodeActing
        <*> pure nodeConfig
        <*> pure mbDBModel
        <*> L.newVar False
        <*> L.newVar False

    let dbUsageFailed = useDb && stopOnDbErr && isNothing mbDBModel

    unless dbUsageFailed
        $ L.logInfo $ "Genesis block (" +|| D.genesisHash ||+ "): " +|| D.genesisKBlock ||+ "."
    
    if dbUsageFailed
        then pure $ Left "Database error."
        else pure $ Right nodeData
