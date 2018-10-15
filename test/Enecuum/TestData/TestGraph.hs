
{-# LANGUAGE TemplateHaskell #-}

module Enecuum.TestData.TestGraph where

import Enecuum.Prelude

import qualified Data.HGraph.THGraph     as G
import           Data.HGraph.StringHashable (StringHash (..), StringHashable, toHash)
import           Control.Lens.TH (makeLenses)

import qualified Data.ByteString.Base64  as Base64
import qualified Data.Serialize          as S
import qualified Crypto.Hash.SHA256      as SHA


import qualified Enecuum.Language as L
import qualified Enecuum.Domain as D
import           Enecuum.Core.HGraph.Interpreters.IO (runHGraphIO)
import           Enecuum.Core.HGraph.Internal.Impl (initHGraph)

data Transaction = Transaction
    { _prevHash    :: StringHash
    , _change      :: BalanceChange
    }
  deriving ( Generic, Show, Eq, Ord, Read)
type Balance = Int
type BalanceChange = Int

instance Serialize Transaction
instance StringHashable Transaction where
    toHash = StringHash . Base64.encode . SHA.hash . S.encode

makeLenses ''Transaction

type TestGraphVar = D.TGraph Transaction
type TestGraphL a = L.HGraphL Transaction a


nilHash :: StringHash
nilHash = toHash (Transaction (toHash @Int 0) 0)

nilTransaction :: Transaction
nilTransaction = Transaction nilHash 0

nilTransactionHash :: D.StringHash
nilTransactionHash = D.toHash nilTransaction

initTestGraph :: IO TestGraphVar
initTestGraph = do
    graph <- initHGraph
    runHGraphIO graph $ L.newNode nilTransaction
    pure graph

-- | Checks if new balance is valid and adds new transaction node.
-- Returns new node hash and new balance.
tryAddTransaction' :: D.StringHash -> Balance -> BalanceChange -> TestGraphL (Maybe (D.StringHash, Balance))
tryAddTransaction' lastNodeHash lastBalance change
    | lastBalance + change < 0 = pure Nothing
    | otherwise = do
        let newTrans     = Transaction lastNodeHash change
        let newTransHash = D.toHash newTrans
        L.newNode newTrans
        L.newLink lastNodeHash newTransHash
        pure $ Just (newTransHash, lastBalance + change)
