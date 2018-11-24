{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE TemplateHaskell        #-}

module Enecuum.Assets.Nodes.GraphNode.Logic where

import           Control.Lens                          (makeFieldsNoPrefix)
import           Data.HGraph.StringHashable
import qualified Data.Map                              as Map
import           Enecuum.Prelude
import           System.FilePath                       ((</>))

import qualified Enecuum.Assets.Nodes.CLens            as CLens
import           Enecuum.Assets.Nodes.GraphNode.Config
import           Enecuum.Assets.Nodes.Messages
import qualified Enecuum.Assets.System.Directory       as L
import qualified Enecuum.Blockchain.DB                 as D
import qualified Enecuum.Blockchain.Lens               as Lens
import qualified Enecuum.Domain                        as D
import           Enecuum.Framework.Language.Extra      (HasStatus, NodeStatus (..))
import qualified Enecuum.Language                      as L

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

-- | RestoreFromDB command.
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
acceptKBlock :: GraphNodeData -> D.KBlock -> connection -> L.NodeL ()
acceptKBlock nodeData kBlock _ = acceptKBlock' nodeData kBlock

acceptMBlock' :: GraphNodeData -> D.Microblock -> L.NodeL ()
acceptMBlock' nodeData mBlock = do
    let microblockValid  = L.verifyMicroblock mBlock
    unless microblockValid $ do
        L.logInfo $ "Microblock is rejected: " +|| toHash mBlock ||+ "."
        L.logInfo $ "Microblock has " +|| toHash mBlock ||+ " invalid signature."
    when microblockValid $ do
        L.logInfo $ "Microblock " +|| toHash mBlock ||+ " is accepted."
        let bData = nodeData ^. blockchain
        let wndGraph = bData ^. Lens.windowedGraph
        void $ L.atomically $ do
            void $ L.addMBlock wndGraph mBlock
            let tx = mBlock ^. Lens.transactions
            let fun :: D.Transaction -> D.TransactionPending -> D.TransactionPending
                fun t = Map.delete (toHash t)
            forM_ tx (L.modifyVar (bData ^. Lens.transactionPending) . fun)
    forM_ (mBlock ^. Lens.transactions) (\tx -> unless (L.verifyTransaction tx) $
        L.logInfo $ "Transaction "  +|| D.showTx tx ||+ "of microblock " +|| toHash mBlock ||+ "has invalid signature.")

-- | Accept mBlock
acceptMBlock :: GraphNodeData -> D.Microblock -> connection -> L.NodeL ()
acceptMBlock nodeData mBlock _ = acceptMBlock' nodeData mBlock

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
getLastKBlock nodeData _ =
    L.atomically $ L.getTopKBlock $ nodeData ^. blockchain . Lens.windowedGraph

getBalance :: GraphNodeData -> GetWalletBalance -> L.NodeL (Either Text WalletBalanceMsg)
getBalance nodeData (GetWalletBalance wallet) = do
    L.logInfo $ "Requested balance for wallet " +|| D.showPublicKey wallet ||+ "."
    let bData = nodeData ^. blockchain

    curLedger <- L.readVarIO $ bData ^. Lens.ledger
    case curLedger ^. at wallet of
        Just balance -> pure $ Right $ WalletBalanceMsg wallet balance
        _            -> pure $ Left $ "Wallet " +|| D.showPublicKey wallet ||+ " does not exist in graph."

getChainLength :: GraphNodeData -> L.NodeL D.BlockNumber
getChainLength nodeData = do
    topKBlock <- L.atomically $ L.getTopKBlock $ nodeData ^. blockchain . Lens.windowedGraph
    pure $ topKBlock ^. Lens.number

acceptGetChainLengthRequest
    :: GraphNodeData -> GetChainLengthRequest -> L.NodeL GetChainLengthResponse
acceptGetChainLengthRequest nodeData _ =
    GetChainLengthResponse <$> getChainLength nodeData

acceptChainFromTo :: GraphNodeData -> GetChainFromToRequest -> L.NodeL (Either Text GetChainFromToResponse)
acceptChainFromTo nodeData (GetChainFromToRequest from to) = do
    L.logInfo $ "Answering chain from " +|| from ||+ " to " +|| to ||+ "."
    let bData = nodeData ^. blockchain
    let wndGraph = bData ^. Lens.windowedGraph

    if from > to
        then pure $ Left "From is greater than to"
        else do
            kBlockList <- L.atomically $ do
                topKBlock <- L.getTopKBlock wndGraph
                chain <- L.findBlocksByNumber wndGraph from topKBlock
                pure $ drop (fromEnum $ (topKBlock ^. Lens.number) - to) chain
            pure $ Right $ GetChainFromToResponse (reverse kBlockList)

getMBlockForKBlocks :: GraphNodeData -> GetMBlocksForKBlockRequest -> L.NodeL (Either Text GetMBlocksForKBlockResponse)
getMBlockForKBlocks nodeData (GetMBlocksForKBlockRequest hash) = do
    L.logInfo $ "Get microblocks for kBlock " +|| hash ||+ "."
    let bData = nodeData ^. blockchain
    GetMBlocksForKBlockResponse <<$>> (L.atomically $ L.getMBlocksForKBlock (bData ^. Lens.windowedGraph) hash)

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

    eKBlocksDb          <- initDb options dbModelPath
    eKBlocksMetaDb      <- initDb options dbModelPath
    eMBlocksDb          <- initDb options dbModelPath
    eMBlocksMetaDb      <- initDb options dbModelPath
    eTransactionsDb     <- initDb options dbModelPath
    eTransactionsMetaDb <- initDb options dbModelPath

    let eModel = D.DBModel
            <$> eKBlocksDb
            <*> eKBlocksMetaDb
            <*> eMBlocksDb
            <*> eMBlocksMetaDb
            <*> eTransactionsDb
            <*> eTransactionsMetaDb

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

    windowSize       <- L.newVarIO 1
    bottomKBlockHash <- L.newVarIO D.genesisHash
    topKBlockHash    <- L.newVarIO D.genesisHash

    let windowedGraph = D.WindowedGraph
          { D._graph        = g
          , D._windowSize   = windowSize
          , D._bottomKBlockHash = bottomKBlockHash
          , D._topKBlockHash    = topKBlockHash
          }

    kBlockPending      <- L.newVarIO Map.empty
    transactionPending <- L.newVarIO Map.empty
    ledger             <- L.newVarIO Map.empty

    nodeData <- L.atomically
        $  GraphNodeData
            ( D.BlockchainData
                  { D._windowedGraph      = windowedGraph
                  , D._kBlockPending      = kBlockPending
                  , D._transactionPending = transactionPending
                  , D._ledger             = ledger
              }
            )
        <$> L.newVar NodeActing
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

synchronize :: GraphNodeData -> Synchronize -> L.NodeL ()
synchronize nodeData (Synchronize addressSync) = graphSynchro nodeData addressSync

data ResultOfChainCompair
    = NeedToSync (D.BlockNumber, D.BlockNumber)
    | ErrorInRequest Text
    | ChainsAreEqual
    | MyChainIsLonger

compareChainLength :: GraphNodeData -> D.Address -> L.NodeL ResultOfChainCompair
compareChainLength nodeData address = do
    eLngth <- L.makeRpcRequest address GetChainLengthRequest
    case eLngth of
        Right (GetChainLengthResponse otherLength)-> do
            lengthOfMyChain <- getChainLength nodeData
            pure $ if
                | otherLength > lengthOfMyChain  -> NeedToSync (lengthOfMyChain, otherLength)
                | otherLength == lengthOfMyChain -> ChainsAreEqual
                | otherwise                      -> MyChainIsLonger
        Left err -> pure $ ErrorInRequest err

syncCurrentMacroBlock :: GraphNodeData -> D.Address -> L.NodeL Bool
syncCurrentMacroBlock nodeData address = do
    let bData = nodeData ^. blockchain
    let wndGraph = bData ^. Lens.windowedGraph
    topNodeHash <- L.readVarIO $ bData ^. Lens.wTopKBlockHash
    eMBlocks <- L.makeRpcRequest address (GetMBlocksForKBlockRequest topNodeHash)
    case eMBlocks of
        Right (GetMBlocksForKBlockResponse mBlocks) -> do
            L.logInfo $ "Mblocks received for kBlock " +|| topNodeHash ||+ " : " +|| mBlocks ||+ "."
            L.atomically $ forM_ mBlocks (L.addMBlock wndGraph)
        Left err -> L.logError $ "Error of sync of current macroblock: " <> err
    pure $ isRight eMBlocks

getKBlockChain :: D.Address -> D.BlockNumber -> D.BlockNumber -> L.NodeL [D.KBlock]
getKBlockChain address curChainLength otherLength = do
    eChain <- L.makeRpcRequest address (GetChainFromToRequest (curChainLength + 1) otherLength)
    case eChain of
        Right (GetChainFromToResponse chainTail) -> do
            L.logInfo $ "Chain tail received from " +|| (curChainLength + 1) ||+ " to " +|| otherLength ||+ " : " +|| chainTail ||+ "."
            pure chainTail
        Left err -> do
            L.logError $ "Error in receiving of kblock chain" <> err
            pure []

tryTakeMBlockChain :: [D.KBlock] -> GraphNodeData -> D.Address -> L.NodeL ()
tryTakeMBlockChain (kBlock:kBlocks) nodeData address = do
    void $ L.addKBlock (nodeData ^. blockchain) kBlock
    whenM (syncCurrentMacroBlock nodeData address) $
        tryTakeMBlockChain kBlocks nodeData address
tryTakeMBlockChain _ _ _ = pure ()

graphSynchro :: GraphNodeData -> D.Address -> L.NodeL ()
graphSynchro nodeData address = compareChainLength nodeData address >>= \case
    NeedToSync (curChainLength, otherLength) -> do
        ok <- syncCurrentMacroBlock nodeData address
        when ok $ do
            chainTail <- getKBlockChain address curChainLength otherLength
            tryTakeMBlockChain chainTail nodeData address
    _ -> pure ()


shrinkGraphWindow :: GraphNodeData -> D.BlockNumber -> L.NodeL ()
shrinkGraphWindow nodeData wndSizeThreshold = do
    -- Separate atomic block to see if the cut is needed.
    -- It might be the cutting process is very long and can happen to rare,
    -- so logs from it can be seen very rare too. But we need to know the process
    -- is going.
    wndSize <- L.readVarIO (nodeData ^. blockchain . Lens.wWindowSize)
    when (wndSize > wndSizeThreshold) $ L.logInfo
        $  "Current graph window: " <> show wndSize
        <> " exceeds wndSizeThreshold: " <> show wndSizeThreshold

    L.atomically $ do
      let bData = nodeData ^. blockchain
      let wndGraph = bData ^. Lens.windowedGraph

      -- Double check is needed here.
      (wndSize, topKBlockHash) <- (,)
          <$> L.readVar (wndGraph ^. Lens.windowSize)
          <*> L.readVar (wndGraph ^. Lens.topKBlockHash)

      mbKNode <- L.getKBlockNode wndGraph topKBlockHash

      case (wndSize > wndSizeThreshold, mbKNode) of
          (False, _)                     -> pure ()
          (_, Nothing)                   -> L.logError $ "Node not found: " <> show topKBlockHash
          (True, Just kNode@(kBlock, _)) -> do
              let bottomKBlockNumber = 1 + (kBlock ^. Lens.number) - wndSizeThreshold
              L.logInfo $ "Making cut. New bottom KBlock number: " <> show bottomKBlockNumber
              L.shrinkGraph bData wndSizeThreshold bottomKBlockNumber kNode
