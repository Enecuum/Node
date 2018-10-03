module Enecuum.TestData.TestGraph where

import Enecuum.Prelude

import           Data.HGraph.StringHashable (StringHash, toHash)

import qualified Enecuum.Language as L
import qualified Enecuum.Domain as D
import           Enecuum.Core.HGraph.Interpreters.IO (runHGraphIO)
import           Enecuum.Core.HGraph.Internal.Impl (initHGraph)

type TestGraphVar = D.TGraph D.Transaction
type TestGraphL a = L.HGraphL D.Transaction a

nilHash :: StringHash
nilHash = toHash (D.Transaction (toHash @Int 0) 0)

nilTransaction :: D.Transaction
nilTransaction = D.Transaction nilHash 0

nilTransactionHash :: D.StringHash
nilTransactionHash = D.toHash nilTransaction

initTestGraph :: IO TestGraphVar
initTestGraph = do
    graph <- initHGraph
    runHGraphIO graph $ L.newNode nilTransaction
    pure graph

-- | Checks if new balance is valid and adds new transaction node.
-- Returns new node hash and new balance.
tryAddTransaction'
  :: D.StringHash
  -> D.Balance
  -> D.BalanceChange
  -> TestGraphL (Maybe (D.StringHash, D.Balance))
tryAddTransaction' lastNodeHash lastBalance change
  | lastBalance + change < 0 = pure Nothing
  | otherwise = do
      let newTrans = D.Transaction lastNodeHash change
      let newTransHash = D.toHash newTrans
      L.newNode newTrans
      L.newLink lastNodeHash newTransHash
      pure $ Just (newTransHash, lastBalance + change)
