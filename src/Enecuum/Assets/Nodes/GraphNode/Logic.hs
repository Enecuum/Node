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
import qualified Enecuum.Language                 as L
import qualified Enecuum.Blockchain.Lens          as Lens
import qualified Enecuum.Blockchain.DB            as D
import           Enecuum.Assets.Nodes.Messages
import           Enecuum.Assets.Nodes.GraphNode.Config
import qualified Enecuum.Assets.Nodes.CLens       as CLens
import qualified Enecuum.Assets.System.Directory  as L

data GraphNodeData = GraphNodeData
    { _blockchain          :: D.BlockchainData
    , _status              :: D.StateVar NodeStatus
    , _config              :: NodeConfig GraphNode
    , _db                  :: Maybe D.DBModel
    , _dumpToDBSignal      :: D.StateVar Bool
    , _restoreFromDBSignal :: D.StateVar Bool
    , _checkPendingSignal  :: D.StateVar Bool
    }

makeFieldsNoPrefix ''GraphNodeData

transactionsToTransfer :: Int
transactionsToTransfer = 20

-- | DumpToDB command.
handleDumpToDB :: GraphNodeData -> DumpToDB -> L.NodeL (Either Text SuccessMsg)
handleDumpToDB nodeData _ = do
    L.writeVarIO (nodeData ^. dumpToDBSignal) True
    pure $ Right SuccessMsg

-- | DumpToDB command.
handleRestoreFromDB :: GraphNodeData -> RestoreFromDB -> L.NodeL (Either Text SuccessMsg)
handleRestoreFromDB nodeData _ = do
    L.writeVarIO (nodeData ^. restoreFromDBSignal) True
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
            L.atomically $ L.modifyVar (bData ^. Lens.transactionPending) (Map.insert (toHash tx ) tx)
            pure $ Right SuccessMsg
        else do
            L.logInfo "Transaction signature is not genuine"
            L.logInfo "Transaction is not accepted"
            pure $ Left "Transaction signature is not genuine. Transaction is not accepted."

-- | Accept kBlock
acceptKBlock' :: GraphNodeData -> D.KBlock -> L.NodeL ()
acceptKBlock' nodeData kBlock = do
    L.logInfo $ "Accepting KBlock (" +|| toHash kBlock ||+ "): " +|| kBlock ||+ "."
    let bData = nodeData ^. blockchain
    kBlockAdded <- L.addKBlock bData kBlock
    L.writeVarIO (nodeData ^. checkPendingSignal) kBlockAdded

-- | Accept kBlock
acceptKBlock :: GraphNodeData -> D.KBlock -> D.Connection D.Udp -> L.NodeL ()
acceptKBlock nodeData kBlock _ = acceptKBlock' nodeData kBlock

-- | Accept mBlock
acceptMBlock :: GraphNodeData -> D.Microblock -> D.Connection D.Udp -> L.NodeL ()
acceptMBlock nodeData mBlock _ = do
    let res@(valid, _, _) = L.verifyMicroblockWithTx mBlock
    unless valid $ printInvalidSignatures res
    when valid $ do
        L.logInfo $ "Microblock " +|| toHash mBlock ||+ " is accepted."
        let bData = nodeData ^. blockchain
        void $ L.atomically $ do
            void $ L.addMBlock bData mBlock
            let tx = mBlock ^. Lens.transactions
            let fun :: D.Transaction -> D.TransactionPending -> D.TransactionPending
                fun t = Map.delete (toHash t)
            forM_ tx (L.modifyVar (bData ^. Lens.transactionPending) . fun)
    where
        printInvalidSignatures :: (Bool, Bool, [Bool]) -> L.NodeL ()
        printInvalidSignatures (valid, mBlockValid, txsValid) = do
            unless valid                 $ L.logInfo $ "Microblock is rejected: " +|| toHash mBlock ||+ "."
            unless mBlockValid           $ L.logInfo $ "Microblock has " +|| toHash mBlock ||+ " invalid signature."
            when (False `elem` txsValid) $ L.logInfo $ "Microblock " +|| toHash mBlock ||+ " transactions have invalid signature."

getKBlockPending :: GraphNodeData -> GetKBlockPending -> L.NodeL D.KBlockPending
getKBlockPending nodeData _ = L.readVarIO $ nodeData ^. blockchain . Lens.kBlockPending

processKBlockPending' :: GraphNodeData -> L.NodeL Bool
processKBlockPending' nodeData = L.processKBlockPending $ nodeData ^. blockchain

getTransactionPending :: GraphNodeData -> GetTransactionPending -> L.NodeL [D.Transaction]
getTransactionPending nodeData _ = do
    let bData = nodeData ^. blockchain
    L.atomically $ do
        trans <- L.readVar $ bData ^. Lens.transactionPending
        pure $ map snd $ take transactionsToTransfer $ Map.toList trans

getLastKBlock :: GraphNodeData -> GetLastKBlock -> L.NodeL D.KBlock
getLastKBlock nodeData _ = do
    let bData = nodeData ^. blockchain
    -- L.logInfo "Top KBlock requested."
    L.atomically $ L.getTopKeyBlock bData
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
    let bData = nodeData ^. blockchain
--    L.logInfo "Answering chain length"
    topKBlock <- L.atomically $ L.getTopKeyBlock bData
    pure $ GetChainLengthResponse $ topKBlock ^. Lens.number


acceptChainFromTo :: GraphNodeData -> GetChainFromToRequest -> L.NodeL (Either Text GetChainFromToResponse)
acceptChainFromTo nodeData (GetChainFromToRequest from to) = do
    L.logInfo $ "Answering chain from " +|| (show from :: Text) ||+ " to " +|| show to
    let bData = nodeData ^. blockchain
    if from > to
        then pure $ Left "From is greater than to"
        else do
            kBlockList <- L.atomically $ do
                topKBlock <- L.getTopKeyBlock bData
                chain <- L.findBlocksByNumber bData from topKBlock
                pure $ drop (fromEnum $ (topKBlock ^. Lens.number) - to) chain
            pure $ Right $ GetChainFromToResponse (reverse kBlockList)

getMBlockForKBlocks :: GraphNodeData -> GetMBlocksForKBlockRequest -> L.NodeL (Either Text GetMBlocksForKBlockResponse)
getMBlockForKBlocks nodeData (GetMBlocksForKBlockRequest hash) = do
    L.logInfo $ "Get microblocks for kBlock " +|| show hash
    let bData = nodeData ^. blockchain
    mBlockList <- L.atomically $ L.getMBlocksForKBlock bData hash
    case mBlockList of
        Nothing        -> pure $ Left "KBlock doesn't exist"
        Just blockList -> pure $ Right $ GetMBlocksForKBlockResponse blockList


initDb :: forall db. D.DB db => D.DBOptions -> FilePath -> L.NodeL (D.DBResult (D.Storage db))
initDb options dbModelPath = do
    let dbPath   = dbModelPath </> D.getDbName @db <> ".db"
    let dbConfig = D.DBConfig dbPath options
    eDb <- L.initDatabase dbConfig
    whenRight eDb $ const $ L.logInfo  $ "Database initialized: " +| dbPath |+ ""
    whenLeft  eDb $ \err -> L.logError $ "Database initialization failed." <>
        "\n    Path: " +| dbPath |+ 
        "\n    Error: " +|| err ||+ ""
    pure eDb

initDBModel' :: FilePath -> D.DBOptions -> L.NodeL (Maybe D.DBModel)
initDBModel' dbModelPath options = do
    void $ L.createFilePath dbModelPath

    eKBlocksDb     <- initDb options dbModelPath
    eKBlocksMetaDb <- initDb options dbModelPath

    let eModel = D.DBModel
            <$> eKBlocksDb
            <*> eKBlocksMetaDb

    when (isLeft  eModel) $ L.logError $ "Failed to initialize DB model: " +| dbModelPath |+ "."
    when (isRight eModel) $ L.logInfo  $ "DB model initialized: "          +| dbModelPath |+ "."
    pure $ rightToMaybe eModel

initDBModel :: NodeConfig GraphNode -> L.NodeL (Maybe D.DBModel)
initDBModel nodeConfig = do

    parentDir <- if nodeConfig ^. CLens.useEnqHomeDir
        then L.getEnecuumDir
        else pure ""

    let dbModelPath = parentDir </> (nodeConfig ^. CLens.dbModelName)
    initDBModel' dbModelPath $ nodeConfig ^. CLens.dbOptions

-- | Initialization of graph node
graphNodeInitialization :: NodeConfig GraphNode -> L.NodeDefinitionL (Either Text GraphNodeData)
graphNodeInitialization nodeConfig = L.scenario $ do

    let useDb       = nodeConfig ^. CLens.useDatabase
    let stopOnDbErr = nodeConfig ^. CLens.stopOnDatabaseError

    mbDBModel <- if useDb
        then initDBModel nodeConfig
        else pure Nothing

    g <- L.newGraph
    L.evalGraphIO g $ L.newNode $ D.KBlockContent D.genesisKBlock

    nodeData <- L.atomically
        $  GraphNodeData <$>
            ( D.BlockchainData g
                <$> L.newVar Map.empty
                <*> L.newVar Map.empty
                <*> L.newVar D.genesisHash
                <*> L.newVar Map.empty
            )
        <*> L.newVar NodeActing
        <*> pure nodeConfig
        <*> pure mbDBModel
        <*> L.newVar False
        <*> L.newVar False
        <*> L.newVar False

    let dbUsageFailed = useDb && stopOnDbErr && isNothing mbDBModel

    unless dbUsageFailed
        $ L.logInfo $ "Genesis block (" +|| D.genesisHash ||+ "): " +|| D.genesisKBlock ||+ "."
    
    if dbUsageFailed
        then pure $ Left "Database error."
        else pure $ Right nodeData
