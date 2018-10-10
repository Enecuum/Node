{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Enecuum.Blockchain.Domain.Generate where

import           "cryptonite" Crypto.Random            (MonadRandom)
import           Data.HGraph.StringHashable            (StringHash (..), toHash)
import           Data.List                             (delete)
import           Enecuum.Blockchain.Domain.Crypto
import           Enecuum.Blockchain.Domain.Graph
import           Enecuum.Blockchain.Domain.KBlock
import           Enecuum.Blockchain.Domain.Microblock
import           Enecuum.Blockchain.Domain.Transaction
import           Enecuum.Blockchain.Domain.Types
import qualified Enecuum.Language                      as L
import           Enecuum.Prelude                       hiding (Ordering)

data Ordering = InOrder | RandomOrder
data Boundary = Off | On

-- quantityOfWallets :: Integer
-- quantityOfWallets = 5

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
            pure (kblock : rest)
        else pure []

genKBlock :: StringHash -> Integer -> KBlock
genKBlock prevHash i = KBlock {_prevHash = prevHash, _number = i, _nonce = i, _solver = toHash (i + 3)}

genNTransactions :: Int -> L.NodeL [Transaction]
genNTransactions k = replicateM k $ genTransaction On

dummyTx = Transaction
 {
   _amount = 0,
   _owner = read "QYy3AT4a3Z88MpEoGDixRgxtWW8v3RfSbJLFQEyFZwMe" :: PublicKey,
   _receiver = read "pYeXNM7cn2B6A68rH9PYLCCgrXWiVbucfNW1XMW3Q4G" :: PublicKey
  }

publicKeys1 :: [PublicKey]
publicKeys1 = map (\a -> read a )
    [
    "8fM3up1pPDUgMnYZzKiBpsnrvNopjSoURSnpYbm5aZKz",
    "4vCovnpyuooGBi7t4LcEGeiQYA2pEKc4hixFGRGADw4X",
    "GS5xDwfTffg86Wyv8uy3H4vVQYqTXBFKPxGPy1Ksp2NS",
    "Jh8vrASby8nrVG7N3PLZjqSpbrpXFGmfpMd1nrYifZou",
    "8LZQhs3Z7WiBZbQvTTeXCcCtXfJYtk6RNxxBExo9PEQm"
    ]

privateKeys1 :: [PrivateKey]
privateKeys1 = map (\a -> read a )
    [
          "FDabUqrGEd1i3rfZpqHJkzhvqP9QEpKveoEwmknfJJFa"
        , "DKAJTFr1bFWHE7psYX976YZis1Fqwkh3ikFAgKaw6bWj"
        , "6uU38xA2ucJ2zEqgg1zs5j3U8hx8RL3thVFNmhk3Nbsq"
        , "3n8QPsZwUJxUK85VrgTEuybyj1zDnUeMeovntB5EdqWP"
        , "MzwHKfF4vGsQB2hgcK3MFKY9TaFaUe78NJwQehfjZ5s"
    ]

wallets1 :: [KeyPair]
wallets1 = map (\(pub,priv) -> KeyPair pub priv) $ zip publicKeys1 privateKeys1

genTransaction :: Boundary -> L.NodeL Transaction
genTransaction isFromRange = do
    (ownerKeyPair, receiverKeyPair) <- case isFromRange of
        On -> do
            let quantityOfWallets = fromIntegral $ length wallets1
            ownerIndex <- fromIntegral <$> L.getRandomInt (0, quantityOfWallets - 1)
            let owner = wallets1 !! ownerIndex
            let rest = delete owner wallets1
            receiverIndex <- fromIntegral <$> L.getRandomInt (0, quantityOfWallets - 2)
            let receiver = rest !! receiverIndex
            pure (owner, receiver)
        Off -> do
            owner <- L.generateKeyPair
            receiver <- L.generateKeyPair
            pure (owner, receiver)

    amount <- L.getRandomInt (0, 100)
    
    let owner = getPub ownerKeyPair
        receiver = getPub receiverKeyPair
        currency = ENQ
        tx = TransactionForSign {
            _owner = owner
          , _receiver = receiver
          , _amount = amount
          , _currency = currency}
    signature <- L.sign (getPriv ownerKeyPair) tx

    let transaction = Transaction {
        _owner = owner
      , _receiver = receiver
      , _amount = amount
      , _currency = currency
      , _signature = signature        
    }
    pure transaction

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
            pure (result : rest)
        else pure []