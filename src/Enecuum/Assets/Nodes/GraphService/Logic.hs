{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE TemplateHaskell        #-}

module Enecuum.Assets.Nodes.GraphService.Logic where

import           Control.Lens                                       (makeFieldsNoPrefix)
import           Data.HGraph.StringHashable
import qualified Data.Map                                           as Map
import           Enecuum.Prelude
import           System.FilePath                                    ((</>))

import           Enecuum.Assets.Nodes.GraphService.GraphServiceData
import           Enecuum.Assets.Nodes.Messages
import qualified Enecuum.Assets.System.Directory                    as L
import qualified Enecuum.Blockchain.DB                              as D
import qualified Enecuum.Blockchain.Lens                            as Lens
import qualified Enecuum.Domain                                     as D
import           Enecuum.Framework.Language.Extra                   (HasStatus)
import qualified Enecuum.Language                                   as L

transactionsToTransfer :: Int
transactionsToTransfer = 20

-- | DumpToDB command.
handleDumpToDB :: GraphServiceData -> DumpToDB -> L.NodeL (Either Text SuccessMsg)
handleDumpToDB nodeData _ = do
    L.writeVarIO (nodeData ^. dumpToDBSignal) True
    pure $ Right SuccessMsg

-- | RestoreFromDB command.
handleRestoreFromDB :: GraphServiceData -> RestoreFromDB -> L.NodeL (Either Text SuccessMsg)
handleRestoreFromDB nodeData _ = do
    L.writeVarIO (nodeData ^. restoreFromDBSignal) True
    pure $ Right SuccessMsg

-- | Accept transaction
acceptTransaction :: GraphServiceData -> CreateTransaction -> L.NodeL (Either Text SuccessMsg)
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
            let msg = "Transaction signature is not genuine.\nTransaction is not accepted"
            L.logInfo msg
            pure $ Left msg

-- | Accept kBlock
acceptKBlock' :: GraphServiceData -> D.KBlock -> L.NodeL ()
acceptKBlock' nodeData kBlock = do
    L.logInfo $ "Accepting KBlock (" +|| toHash kBlock ||+ "): " +|| kBlock ||+ "."
    let bData = nodeData ^. blockchain
    kBlockAdded <- L.addKBlock bData kBlock
    L.writeVarIO (nodeData ^. checkPendingSignal) kBlockAdded

-- | Accept kBlock
acceptKBlock :: GraphServiceData -> D.KBlock -> connection -> L.NodeL ()
acceptKBlock nodeData kBlock _ = acceptKBlock' nodeData kBlock

acceptMBlock' :: GraphServiceData -> D.Microblock -> L.NodeL ()
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
acceptMBlock :: GraphServiceData -> D.Microblock -> connection -> L.NodeL ()
acceptMBlock nodeData mBlock _ = acceptMBlock' nodeData mBlock

getKBlockPending :: GraphServiceData -> GetKBlockPending -> L.NodeL D.KBlockPending
getKBlockPending nodeData _ = L.readVarIO $ nodeData ^. blockchain . Lens.kBlockPending

processKBlockPending' :: GraphServiceData -> L.NodeL Bool
processKBlockPending' nodeData = L.processKBlockPending $ nodeData ^. blockchain

getTransactionPending :: GraphServiceData -> GetTransactionPending -> L.NodeL [D.Transaction]
getTransactionPending nodeData _ = do
    let bData = nodeData ^. blockchain
    L.atomically $ do
        trans <- L.readVar $ bData ^. Lens.transactionPending
        pure $ map snd $ take transactionsToTransfer $ Map.toList trans

getLastKBlock :: GraphServiceData -> GetLastKBlock -> L.NodeL D.KBlock
getLastKBlock nodeData _ =
    L.atomically $ L.getTopKBlock $ nodeData ^. blockchain . Lens.windowedGraph

getBalance :: GraphServiceData -> GetWalletBalance -> L.NodeL (Either Text WalletBalanceMsg)
getBalance nodeData (GetWalletBalance wallet) = do
    L.logInfo $ "Requested balance for wallet " +|| D.showPublicKey wallet ||+ "."
    let bData = nodeData ^. blockchain

    curLedger <- L.readVarIO $ bData ^. Lens.ledger
    case curLedger ^. at wallet of
        Just balance -> pure $ Right $ WalletBalanceMsg wallet balance
        _            -> pure $ Left $ "Wallet " +|| D.showPublicKey wallet ||+ " does not exist in graph."

getChainLength :: GraphServiceData -> L.NodeL D.BlockNumber
getChainLength nodeData = do
    topKBlock <- L.atomically $ L.getTopKBlock $ nodeData ^. blockchain . Lens.windowedGraph
    pure $ topKBlock ^. Lens.number

acceptGetChainLengthRequest
    :: GraphServiceData -> GetChainLengthRequest -> L.NodeL GetChainLengthResponse
acceptGetChainLengthRequest nodeData _ =
    GetChainLengthResponse <$> getChainLength nodeData

acceptChainFromTo :: GraphServiceData -> GetChainFromToRequest -> L.NodeL (Either Text GetChainFromToResponse)
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

getMBlockForKBlocks :: GraphServiceData -> GetMBlocksForKBlockRequest -> L.NodeL (Either Text GetMBlocksForKBlockResponse)
getMBlockForKBlocks nodeData (GetMBlocksForKBlockRequest hash) = do
    L.logInfo $ "Get microblocks for kBlock " +|| hash ||+ "."
    let bData = nodeData ^. blockchain
    GetMBlocksForKBlockResponse <<$>> L.atomically (L.getMBlocksForKBlock (bData ^. Lens.windowedGraph) hash)

synchronize :: GraphServiceData -> Synchronize -> L.NodeL ()
synchronize nodeData (Synchronize addressSync) = graphSynchro nodeData addressSync

data ResultOfChainCompare
    = NeedToSync (D.BlockNumber, D.BlockNumber)
    | ErrorInRequest Text
    | ChainsAreEqual
    | MyChainIsLonger

compareChainLength :: GraphServiceData -> D.Address -> L.NodeL ResultOfChainCompare
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

syncCurrentMacroBlock :: GraphServiceData -> D.Address -> L.NodeL Bool
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

tryTakeMBlockChain :: [D.KBlock] -> GraphServiceData -> D.Address -> L.NodeL ()
tryTakeMBlockChain (kBlock:kBlocks) nodeData address = do
    void $ L.addKBlock (nodeData ^. blockchain) kBlock
    whenM (syncCurrentMacroBlock nodeData address) $
        tryTakeMBlockChain kBlocks nodeData address
tryTakeMBlockChain _ _ _ = pure ()

graphSynchro :: GraphServiceData -> D.Address -> L.NodeL ()
graphSynchro nodeData address = compareChainLength nodeData address >>= \case
    NeedToSync (curChainLength, otherLength) -> do
        ok <- syncCurrentMacroBlock nodeData address
        when ok $ do
            chainTail <- getKBlockChain address curChainLength otherLength
            tryTakeMBlockChain chainTail nodeData address
    _ -> pure ()

-- | Shrinking graph beside the window.
-- N.B. All these functions are living in separate transactions
-- Becase the transactions should be small as possible.
shrinkGraphWindow :: GraphServiceData -> D.BlockNumber -> L.NodeL ()
shrinkGraphWindow nodeData windowSize = do
    let bData    = nodeData ^. blockchain
    let wndGraph = bData    ^. Lens.windowedGraph

    (bottomKBlockHash, topKBlockHash) <- (,)
        <$> L.readVarIO (wndGraph ^. Lens.bottomKBlockHash)
        <*> L.readVarIO (wndGraph ^. Lens.topKBlockHash)

    (mbBottomKNode, mbTopKNode) <- L.atomically $ (,)
        <$> (L.getKBlockNode wndGraph bottomKBlockHash)
        <*> (L.getKBlockNode wndGraph topKBlockHash)

    case (mbBottomKNode, mbTopKNode) of
        (Nothing, _) -> L.logError $ "Node not found: " <> show bottomKBlockHash
        (_, Nothing) -> L.logError $ "Node not found: " <> show topKBlockHash
        (Just bottomKNode, Just topKNode) -> do
            let bottomNumber    = L.getKNodeNumber bottomKNode
            let topNumber       = L.getKNodeNumber topKNode
            let newBottomNumber = 1 + topNumber - windowSize
            let wndSize         = 1 + topNumber - bottomNumber

            when (wndSize > windowSize) $ do
                L.logInfo $  "Current graph window: " <> show wndSize
                          <> " exceeds windowSize: " <> show windowSize

                mbNewBottomKNode <- L.atomically $
                    L.findKBlockNodeDownward' wndGraph (L.FindByNumber newBottomNumber) (Just topKNode)

                whenJust mbNewBottomKNode $ \newBottomKNode -> do
                    let newBottomKBlockHash = L.getKNodeHash newBottomKNode

                    L.logInfo $ "New bottom kBlock [" +|| newBottomNumber ||+ "]: "
                            <> show newBottomKBlockHash
                            <> "\n    Making cut."
                    -- Do not merge these distinct atomic blocks.
                    L.atomically $ L.writeVar (wndGraph ^. Lens.bottomKBlockHash) newBottomKBlockHash
                    L.atomically $ L.shrinkGraphDownFrom wndGraph (snd newBottomKNode)
