module Enecuum.Blockchain.Domain.Generate where

import           Data.HGraph.StringHashable            (StringHash (..), toHash)
import           Data.List                             (delete)
import           Enecuum.Blockchain.Domain.Graph
import           Enecuum.Blockchain.Domain.KBlock
import           Enecuum.Blockchain.Domain.Microblock
import           Enecuum.Blockchain.Domain.Transaction
import qualified Enecuum.Language                      as L
import           Enecuum.Prelude                       hiding (Ordering)

data Ordering = InOrder | RandomOrder

kBlockInBunch :: Integer
kBlockInBunch = 3

generateNKBlocks = generateKBlocks genesisHash
generateNKBlocksWithOrder = createKBlocks genesisHash

-- Generate bunch of key blocks (randomly or in order)
createKBlocks :: StringHash -> Integer -> Ordering -> L.NodeL (StringHash, [KBlock])
createKBlocks prevKBlockHash from order = do
    (lastHash, kBlockBunch) <- generateKBlocks prevKBlockHash from
    kBlockIndices           <- generateIndices order
    let kBlocks = map ((kBlockBunch !!) . fromIntegral) kBlockIndices
    pure (lastHash, kBlocks)

-- Generate bunch of key blocks (in order)
generateKBlocks :: StringHash -> Integer -> L.NodeL (StringHash, [KBlock])
generateKBlocks prevHash from = do
    blocks <- loopGenKBlock prevHash from (from + kBlockInBunch)
    case blocks of
        [] -> pure (prevHash, [])
        _  -> pure (toHash $ last blocks, blocks)

-- loop - state substitute : create new Kblock using hash of previous
loopGenKBlock :: StringHash -> Integer -> Integer -> L.NodeL [KBlock]
loopGenKBlock prevHash from to = do
    let kblock      = genKBlock prevHash from
        newPrevHash = toHash kblock
    if (from < to)
        then do
            rest <- loopGenKBlock newPrevHash (from + 1) to
            return (kblock : rest)
        else return []

genKBlock :: StringHash -> Integer -> KBlock
genKBlock prevHash i = KBlock {_prevHash = prevHash, _number = i, _nonce = i, _solver = toHash (i + 3)}

genNTransactions :: Int -> L.NodeL [Transaction]
genNTransactions k = replicateM k genTransaction

genTransaction :: L.NodeL Transaction
genTransaction = do
    owner <- L.getRandomInt (1, 5)
    let rest = delete owner [1 .. 5]
    receiverIndex <- fromIntegral <$> L.getRandomInt (0, 3)
    let receiver = rest !! receiverIndex
    amount <- L.getRandomInt (0, 100)
    pure Transaction {_owner = owner, _receiver = receiver, _amount = amount}

genMicroblock :: StringHash -> [Transaction] -> Microblock
genMicroblock hashofKeyBlock tx = Microblock {_keyBlock = hashofKeyBlock, _transactions = tx}

genRandMicroblock :: KBlock -> L.NodeL Microblock
genRandMicroblock kBlock = genMicroblock (toHash kBlock) <$> genNTransactions 3

generateIndices :: Ordering -> L.NodeL [Integer]
generateIndices order = do
    case order of
        RandomOrder -> loopGenIndices [0 .. kBlockInBunch]
        InOrder     -> pure $ [0 .. kBlockInBunch]

-- loop: choose randomly one from the rest of list Integers
-- example:
-- [1,2,3,4,5] - 2
-- [1,3,4,5] - 4
-- [1,3,5] - 5
-- [1,3] - 1
-- [3] - 3
-- the result: [2,4,5,1,3]
loopGenIndices :: [Integer] -> L.NodeL [Integer]
loopGenIndices numbers = do
    if (not $ null numbers)
        then do
            let maxIndex = fromIntegral $ length numbers - 1
            p <- fromIntegral <$> L.getRandomInt (0, maxIndex)
            let result = numbers !! p
            -- choose next number from rest
            rest <- loopGenIndices $ delete result numbers
            return (result : rest)
        else return []
