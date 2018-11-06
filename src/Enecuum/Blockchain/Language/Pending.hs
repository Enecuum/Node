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
import qualified Enecuum.Framework.LogState               as Log
import           Enecuum.Prelude

-- | Checks if the kBlock valid.
-- N.B.: it doesn't support forks.
-- TODO: support forks.
validateKBlock :: D.StateVar [Text] -> BlockchainData -> D.KBlock -> L.StateL D.KBlockValidity
validateKBlock logV bData kBlock = do
    topKBlock <- L.getTopKeyBlock logV bData

    let nextBlock    = L.kBlockIsNext kBlock topKBlock
    let prevBlock    = L.kBlockExists kBlock topKBlock
    let futureKBlock = not nextBlock && not prevBlock

    if  | nextBlock    -> pure D.NextKBlock
        | prevBlock    -> pure D.PreviousKBlock
        | futureKBlock -> pure D.FutureKBlock

data KBlockResult
    = KBlockAdded
    | KBlockPostponed
    | KBlockErrored

addTopKBlock' :: D.StateVar [Text] -> D.BlockchainData -> D.KBlock -> L.StateL KBlockResult
addTopKBlock' logV bData kBlock = do
    res <- L.addTopKBlock "[Network]" logV bData kBlock
    pure $ if res then KBlockAdded else KBlockErrored

-- | If KBlock is valid, adds it to the graph or to the pending.
-- If it's not valid (for example, exists already), drops it.
-- Returns True if Pending needs to be processed.
addKBlock :: D.StateVar [Text] -> BlockchainData -> D.KBlock -> L.NodeL Bool
addKBlock logV bData kBlock = do
    
    res <- L.atomically $ do
        validity <- validateKBlock logV bData kBlock
        case validity of
            D.NextKBlock   -> addTopKBlock' logV bData kBlock
            D.FutureKBlock -> pure KBlockPostponed
            _              -> pure KBlockErrored
    
    case res of
        KBlockErrored   -> L.logError ("Error in processing KBlock: " +|| kBlock ||+ ".") >> pure False
        KBlockAdded     -> pure True
        KBlockPostponed -> L.atomically (addBlockToPending logV bData kBlock) >> pure False


-- | Move one block from pending to graph if possible and remove it from pending.
--   Return true if function had effect.
processKBlockPending :: D.StateVar [Text] -> BlockchainData -> L.NodeL Bool
processKBlockPending logV bData = L.atomically $ do
    topKBlock <- L.getTopKeyBlock logV bData
    pending   <- L.readVar (_kBlockPending bData)

    let nextKBlockNumber = topKBlock ^. Lens.number + 1
    case Map.lookup nextKBlockNumber pending of
        Nothing     -> pure False
        Just kBlock -> do
            let newPending = Map.delete nextKBlockNumber pending
            L.writeVar (_kBlockPending bData) newPending
            L.addTopKBlock "[Pending]" logV bData kBlock

-- | Add new key block to pending.
addBlockToPending :: D.StateVar [Text] -> BlockchainData -> D.KBlock -> L.StateL ()
addBlockToPending logV bData kBlock = do
    Log.stateLog logV $ "Adding KBlock to pending: " +|| kBlock ||+ ""
    L.modifyVar (_kBlockPending bData) (Map.insert (kBlock ^. Lens.number) kBlock)

-- | Add new transaction to pending.
addTransactionToPending :: D.StateVar [Text] -> BlockchainData -> D.Transaction -> L.StateL Bool
addTransactionToPending logV bData transaction = do
    Log.stateLog logV "Add transaction to pending"
    L.modifyVar (_transactionPending bData) ( Map.insert (D.toHash transaction) transaction )
    pure True
