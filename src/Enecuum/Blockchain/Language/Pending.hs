module Enecuum.Blockchain.Language.Pending where

import Enecuum.Prelude
import Data.Map
import qualified Enecuum.Framework.Language as L
import qualified Enecuum.Core.Language as L
import qualified Enecuum.Framework.Domain as D
import qualified Enecuum.Core.Types as D
import Enecuum.Blockchain.Domain.BlockchainData (BlockchainData(..))
import qualified Enecuum.Blockchain.Domain.Graph as D
import qualified Enecuum.Blockchain.Domain.KBlock as D
import  Enecuum.Blockchain.Domain.Microblock  (Microblock(..))
import  Enecuum.Blockchain.Domain.Transaction (Transaction(..))
import qualified Enecuum.Blockchain.Language.Graph as L
import qualified Enecuum.Framework.LogState as Log


-- | Move one block from pending to graph if it is possibly and remove it from pending.
--   Return true if function had effect.
moveKBlockToGraph :: D.StateVar [Text] -> BlockchainData -> L.StateL Bool
moveKBlockToGraph logV bData = do
    topKBlock <- L.getTopKeyBlock logV bData
    pending   <- L.readVar (_kBlockPending bData)
    case pending of
        kBlock : newPending | L.kBlockIsNext kBlock topKBlock -> do
            L.writeVar (_kBlockPending bData) newPending
            Log.stateLog logV "Moving KBlock from pending to graph."
            L.addKBlock logV bData kBlock
        _ -> pure False


-- | Add new key block to pending.
addBlockToPending :: D.StateVar [Text] -> BlockchainData -> D.KBlock -> L.StateL Bool
addBlockToPending logV bData kBlock = do
    Log.stateLog logV "Adding KBlock to pending"
    L.modifyVar (_kBlockPending bData) (\pending -> sortOn (D._number) $ kBlock : pending)
    pure True