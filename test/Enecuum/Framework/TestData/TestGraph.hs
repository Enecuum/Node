module Enecuum.Framework.TestData.TestGraph where

import Enecuum.Prelude

import qualified Data.HGraph.THGraph     as G
import           Data.HGraph.StringHashable (StringHash, toHash)

import qualified Enecuum.Language as L
import qualified Enecuum.Domain as D
import           Enecuum.Core.HGraph.Interpreters.IO (initHGraph, runHGraph)

type LGraph = TVar (G.THGraph D.Transaction)

nilHash :: StringHash
nilHash = toHash (D.Transaction (toHash @Int 0) 0)

nilTransaction :: D.Transaction
nilTransaction = D.Transaction nilHash 0

nilTransactionHash :: D.StringHash
nilTransactionHash = D.toHash nilTransaction

initLGraph :: IO LGraph
initLGraph = do
    graph <- initHGraph
    runHGraph graph $ L.newNode nilTransaction
    pure graph

type Balance = Int
type BalanceChange = Int

-- | Checks if new balance is valid and adds new transaction node.
-- Returns new node hash and new balance.
tryAddTransaction'
  :: D.StringHash
  -> Balance
  -> BalanceChange
  -> L.GraphModel (Maybe (D.StringHash, Balance))
tryAddTransaction' lastNodeHash lastBalance change
  | lastBalance + change < 0 = pure Nothing
  | otherwise = do
      let newTrans = D.Transaction lastNodeHash change
      let newTransHash = D.toHash newTrans
      L.newNode newTrans
      L.newLink lastNodeHash newTransHash
      pure $ Just (newTransHash, lastBalance + change)