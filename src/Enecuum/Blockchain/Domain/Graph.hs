-- TODO: this is copy-paste from tests with little changes.

module Enecuum.Blockchain.Domain.Graph where

import Enecuum.Prelude

import qualified Data.HGraph.THGraph     as G
import           Data.HGraph.StringHashable (StringHash, toHash)

import qualified Enecuum.Language as L
import qualified Enecuum.Core.Types as D
import qualified Enecuum.Blockchain.Domain.Transaction as D
import           Enecuum.Core.HGraph.Interpreters.IO (runHGraphIO)
import           Enecuum.Core.HGraph.Internal.Impl (initHGraph)

type GraphVar = D.TGraph D.Transaction
type GraphL a = L.HGraphL D.Transaction a

nilHash :: StringHash
nilHash = toHash (D.Transaction (toHash @Int 0) 0)

nilTransaction :: D.Transaction
nilTransaction = D.Transaction nilHash 0

nilTransactionHash :: D.StringHash
nilTransactionHash = D.toHash nilTransaction

initGraph :: IO GraphVar
initGraph = do
    graph <- initHGraph
    runHGraphIO graph $ L.newNode nilTransaction
    pure graph

-- | Checks if new balance is valid and adds new transaction node.
-- Returns new node hash and new balance.
tryAddTransaction'
  :: D.StringHash
  -> D.Balance
  -> D.BalanceChange
  -> GraphL (Maybe (D.StringHash, D.Balance))
tryAddTransaction' lastNodeHash lastBalance change
  | lastBalance + change < 0 = pure Nothing
  | otherwise = do
      let newTrans = D.Transaction lastNodeHash change
      let newTransHash = D.toHash newTrans
      L.newNode newTrans
      L.newLink lastNodeHash newTransHash
      pure $ Just (newTransHash, lastBalance + change)
