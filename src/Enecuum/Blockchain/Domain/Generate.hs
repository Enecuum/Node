module Enecuum.Blockchain.Domain.Generate where

import Enecuum.Prelude
import Enecuum.Blockchain.Domain.Graph
import Enecuum.Blockchain.Domain.KBlock
import Enecuum.Blockchain.Domain.Transaction
import Enecuum.Blockchain.Domain.Microblock
import Data.HGraph.StringHashable (StringHash (..),toHash)
import qualified Enecuum.Language              as L


kBlockInBunch :: Integer
kBlockInBunch = 5

generateNKBlocks = generateKBlocks genesisHash

-- Generate bunch of key blocks
generateKBlocks :: StringHash -> Integer -> Free L.NodeF [KBlock]
generateKBlocks prevHash from = loopGenKBlock prevHash from (from + kBlockInBunch)

-- loop - state substitute : create new Kblock using hash of previous
loopGenKBlock :: StringHash -> Integer -> Integer -> Free L.NodeF [KBlock]
loopGenKBlock prevHash from to = do
  let kblock = genKBlock prevHash from
      newPrevHash = toHash kblock
  if (from < to)
    then do
      rest <- loopGenKBlock newPrevHash (from + 1) to
      return (kblock:rest)
    else return []

genKBlock :: StringHash -> Integer -> KBlock
genKBlock prevHash i = KBlock
    { _prevHash   = prevHash
    , _number     = i
    , _nonce      = i
    , _solver     = toHash (i + 3)
    }

genNTransactions :: Int -> L.NodeL [Transaction]
genNTransactions k =  do
  numbers <- replicateM k $ L.getRandomInt (0, 1000)
  pure $ map genTransaction numbers

genTransaction :: Integer -> Transaction
genTransaction i =  Transaction
    { _owner     = i
    , _receiver  = i + 100
    , _amount    = i + 7
    }

genMicroblock :: StringHash -> [Transaction] -> Microblock
genMicroblock hashofKeyBlock tx = Microblock
    { _keyBlock     = hashofKeyBlock
    , _transactions = tx
    }


