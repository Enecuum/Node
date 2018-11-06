module Enecuum.Blockchain.Language.Pending where

import qualified Data.Map                                 as Map
import qualified Enecuum.Blockchain.Domain                as D
import           Enecuum.Blockchain.Domain.BlockchainData (BlockchainData (..))
import qualified Enecuum.Blockchain.Language.Graph        as L
import qualified Enecuum.Core.Types                       as D
import qualified Enecuum.Framework.Domain                 as D
import qualified Enecuum.Framework.Language               as L
import qualified Enecuum.Framework.LogState               as Log
import           Enecuum.Prelude


-- | If KBlock is valid, adds it to the graph or to the pending.
-- If it's not valid (for example, exists already), drops it.
-- Returns True if KBlock is added somewehere.
addKBlock :: D.StateVar [Text] -> BlockchainData -> D.KBlock -> L.StateL Bool
addKBlock logV bData kBlock = do
    topKBlock <- L.getTopKeyBlock logV bData

    let nextBlock    = L.kBlockIsNext kBlock topKBlock
    let prevBlock    = L.kBlockExists kBlock topKBlock
    let futureKBlock = not nextBlock && not prevBlock

    case (nextBlock, futureKBlock) of
        (True, _) -> L.addTopKBlock    logV bData kBlock >> pure True
        (_, True) -> addBlockToPending logV bData kBlock >> pure True
        _         -> pure False

processPending :: D.StateVar [Text] -> BlockchainData -> L.NodeL ()
processPending logV _ = L.atomically $ Log.stateLog logV "Pending is temporarily disabled."

-- | Move one block from pending to graph if possible and remove it from pending.
--   Return true if function had effect.
moveKBlockToGraph :: D.StateVar [Text] -> BlockchainData -> L.StateL Bool
moveKBlockToGraph logV bData = do
    topKBlock <- L.getTopKeyBlock logV bData
    pending   <- L.readVar (_kBlockPending bData)
    case pending of
        kBlock : newPending | L.kBlockIsNext kBlock topKBlock -> do
            L.writeVar (_kBlockPending bData) newPending
            Log.stateLog logV "Move KBlock from pending to graph."
            L.addTopKBlock logV bData kBlock
        _ -> pure False


-- | Add new key block to pending.
addBlockToPending :: D.StateVar [Text] -> BlockchainData -> D.KBlock -> L.StateL Bool
addBlockToPending logV bData kBlock = do
    Log.stateLog logV "Add KBlock to pending"
    L.modifyVar (_kBlockPending bData) (\pending -> sortOn D._number $ kBlock : pending)
    pure True

-- | Add new transaction to pending.
addTransactionToPending :: D.StateVar [Text] -> BlockchainData -> D.Transaction -> L.StateL Bool
addTransactionToPending logV bData transaction = do
    Log.stateLog logV "Add transaction to pending"
    L.modifyVar (_transactionPending bData) ( Map.insert (D.toHash transaction) transaction )
    pure True
