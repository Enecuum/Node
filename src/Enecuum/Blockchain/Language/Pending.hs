module Enecuum.Blockchain.Language.Pending where

import qualified Data.Map                                 as Map
import qualified Enecuum.Blockchain.Domain                as D
import           Enecuum.Blockchain.Domain.BlockchainData (BlockchainData (..))
import qualified Enecuum.Blockchain.Language.Graph        as L
import qualified Enecuum.Blockchain.Lens                  as Lens
import qualified Enecuum.Core.Types                       as D
import qualified Enecuum.Core.Language                    as L
import qualified Enecuum.Framework.Domain                 as D
import qualified Enecuum.Framework.Language               as L
import           Enecuum.Prelude

-- | Checks if the kBlock valid.
-- N.B.: it doesn't support forks.
-- TODO: support forks.
-- TODO: support check of "not valid k-block"
-- TODO: support check of "already exist k-block"
validateKBlock :: BlockchainData -> D.KBlock -> L.StateL D.KBlockValidity
validateKBlock bData kBlock = do
    topKBlock <- L.getTopKeyBlock bData

    let nextBlock    = L.kBlockIsNext kBlock topKBlock
    let prevBlock    = L.kBlockExists kBlock topKBlock
    let futureKBlock = not nextBlock && not prevBlock

    if  | nextBlock    -> pure D.NextKBlock
        -- FIXME:
        -- it is incorrectly, does this block already exist
        -- or just from a growing branch?
        | prevBlock    -> pure D.PreviousKBlock
        | futureKBlock -> pure D.FutureKBlock

data KBlockResult
    = KBlockAdded
    | KBlockPostponed
    | KBlockAlreadyPresent
    | KBlockErrored
    deriving Eq

addTopKBlock' :: D.BlockchainData -> D.KBlock -> L.StateL KBlockResult
addTopKBlock' bData kBlock = do
    res <- L.addTopKBlock "[Network]" bData kBlock
    pure $ if res then KBlockAdded else KBlockErrored

-- | If KBlock is valid, adds it to the graph or to the pending.
-- If it's not valid (for example, exists already), drops it.
-- Returns True if Pending needs to be processed.
addKBlock :: BlockchainData -> D.KBlock -> L.NodeL Bool
addKBlock bData kBlock = do
    
    res <- L.atomically $ do
        validity <- validateKBlock bData kBlock
        case validity of
            D.NextKBlock     -> addTopKBlock' bData kBlock
            D.FutureKBlock   -> pure KBlockPostponed
            D.PreviousKBlock -> pure KBlockAlreadyPresent
            _                -> pure KBlockErrored
    
    case res of
        KBlockErrored           -> L.logError $ "Error in processing KBlock: "    +|| kBlock ||+ "."
        KBlockAlreadyPresent    -> L.logInfo  $ "KBlock aready exixst en graph: " +|| kBlock ||+ "."
        KBlockAdded             -> pure ()
        KBlockPostponed         -> L.atomically $ addBlockToPending bData kBlock
    pure $ res == KBlockAdded


-- | Move one block from pending to graph if possible and remove it from pending.
--   Return true if function had effect.
processKBlockPending :: BlockchainData -> L.NodeL Bool
processKBlockPending bData = L.atomically $ do
    topKBlock <- L.getTopKeyBlock bData
    pending   <- L.readVar (_kBlockPending bData)

    let nextKBlockNumber = topKBlock ^. Lens.number + 1
    case Map.lookup nextKBlockNumber pending of
        Nothing     -> pure False
        Just kBlock -> do
            let newPending = Map.delete nextKBlockNumber pending
            L.writeVar (_kBlockPending bData) newPending
            L.addTopKBlock "[Pending]" bData kBlock

-- | Add new key block to pending.
addBlockToPending :: BlockchainData -> D.KBlock -> L.StateL ()
addBlockToPending bData kBlock = do
    L.logInfo $ "Adding KBlock to pending: " +|| kBlock ||+ ""
    L.modifyVar (_kBlockPending bData) (Map.insert (kBlock ^. Lens.number) kBlock)

-- | Add new transaction to pending.
addTransactionToPending :: BlockchainData -> D.Transaction -> L.StateL Bool
addTransactionToPending bData transaction = do
    L.logInfo "Add transaction to pending"
    L.modifyVar (_transactionPending bData) (Map.insert (D.toHash transaction) transaction)
    pure True
