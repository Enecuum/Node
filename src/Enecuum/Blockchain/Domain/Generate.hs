{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PackageImports        #-}
module Enecuum.Blockchain.Domain.Generate where

import           Data.HGraph.StringHashable            (StringHash (..), toHash)
import           Data.List                             (delete)
import           Enecuum.Blockchain.Domain.Crypto
import           Enecuum.Blockchain.Domain.KBlock
import           Enecuum.Blockchain.Domain.Microblock
import           Enecuum.Blockchain.Domain.Transaction
import           Enecuum.Blockchain.Domain.Types
import qualified Enecuum.Language                      as L
import           Enecuum.Prelude                       hiding (Ordering)

-- | Order for key blocks
data Ordering = InOrder | RandomOrder

-- | Boundary for transaction generation (On - for demo purpose, Off - for production)
data Boundary = Off | On

kBlockInBunch :: Integer
kBlockInBunch = 3

transactionsInMicroblock :: Int
transactionsInMicroblock = 3

generateNKBlocks :: (L.ERandom m, Monad m) => Integer -> m (StringHash, [KBlock])
generateNKBlocks = generateKBlocks genesisHash

generateNKBlocksWithOrder :: Integer -> Ordering -> L.ERandomL (StringHash, [KBlock])
generateNKBlocksWithOrder = createKBlocks genesisHash

-- Generate bunch of key blocks (randomly or in order)
createKBlocks :: (Monad m, L.ERandom m) => StringHash -> Integer -> Ordering -> m (StringHash, [KBlock])
createKBlocks prevKBlockHash from order = do
    (lastHash, kBlockBunch) <- generateKBlocks prevKBlockHash from
    kBlockIndices           <- generateIndices order
    let kBlocks = map (kBlockBunch !!) kBlockIndices
    pure (lastHash, kBlocks)

-- Generate bunch of key blocks (in order)
generateKBlocks :: Monad m => StringHash -> Integer -> m (StringHash, [KBlock])
generateKBlocks prevHash from = do
    blocks <- loopGenKBlock prevHash from (from + kBlockInBunch)
    case blocks of
        [] -> pure (prevHash, [])
        _  -> pure (toHash $ last blocks, blocks)

-- loop - state substitute : create new Kblock using hash of previous
loopGenKBlock :: Monad m => StringHash -> Integer -> Integer -> m [KBlock]
loopGenKBlock prevHash from to = do
    let kblock      = genKBlock prevHash from
        newPrevHash = toHash kblock
    if (from < to)
        then do
            rest <- loopGenKBlock newPrevHash (from + 1) to
            pure (kblock : rest)
        else pure []

genRandKeyBlock :: (Monad m, L.ERandom m) => m KBlock
genRandKeyBlock = do
    number <- fromIntegral <$> L.getRandomInt (1,1000)
    nonce <- fromIntegral <$> L.getRandomInt (1,1000)
    time <- fromIntegral <$> L.getRandomInt (1,1000)
    r <- L.getRandomInt (1,1000)
    prevHash <- L.getRandomByteString r
    solver <- L.getRandomByteString r
    pure $ KBlock 
        { _prevHash = StringHash prevHash
        , _number = number
        , _nonce = nonce
        , _solver = StringHash solver
        , _time = time
        } 

genKBlock :: StringHash -> Integer -> KBlock
genKBlock prevHash i = KBlock 
    { _prevHash = prevHash
    , _number = i
    , _nonce = i
    , _solver = toHash (i + 3)
    , _time = i
    }

genNTransactions :: (L.ERandom m, Monad m) => Int -> m [Transaction]
genNTransactions k = replicateM k $ genTransaction On

publicKeys1 :: [PublicKey]
publicKeys1 = map read
    [
    "8fM3up1pPDUgMnYZzKiBpsnrvNopjSoURSnpYbm5aZKz",
    "4vCovnpyuooGBi7t4LcEGeiQYA2pEKc4hixFGRGADw4X",
    "GS5xDwfTffg86Wyv8uy3H4vVQYqTXBFKPxGPy1Ksp2NS",
    "Jh8vrASby8nrVG7N3PLZjqSpbrpXFGmfpMd1nrYifZou",
    "8LZQhs3Z7WiBZbQvTTeXCcCtXfJYtk6RNxxBExo9PEQm"
    ]

privateKeys1 :: [PrivateKey]
privateKeys1 = map read
    [
          "FDabUqrGEd1i3rfZpqHJkzhvqP9QEpKveoEwmknfJJFa"
        , "DKAJTFr1bFWHE7psYX976YZis1Fqwkh3ikFAgKaw6bWj"
        , "6uU38xA2ucJ2zEqgg1zs5j3U8hx8RL3thVFNmhk3Nbsq"
        , "3n8QPsZwUJxUK85VrgTEuybyj1zDnUeMeovntB5EdqWP"
        , "MzwHKfF4vGsQB2hgcK3MFKY9TaFaUe78NJwQehfjZ5s"
    ]

-- | Wallets for demo purpose
wallets1 :: [KeyPair]
wallets1 = map (\(pub,priv) -> KeyPair pub priv) $ zip publicKeys1 privateKeys1

-- | Generate signed transaction
genTransaction :: (Monad m, L.ERandom m) => Boundary -> m Transaction
genTransaction isFromRange = do
    (ownerKeyPair, receiverKeyPair) <- case isFromRange of
        On -> do
            let quantityOfWallets = length wallets1
            ownerIndex <- L.getRandomInt (0, quantityOfWallets - 1)
            let owner = wallets1 !! ownerIndex
            let rest = delete owner wallets1
            receiverIndex <- L.getRandomInt (0, quantityOfWallets - 2)
            let receiver = rest !! receiverIndex
            pure (owner, receiver)
        Off -> do
            owner <- L.generateKeyPair
            receiver <- L.generateKeyPair
            pure (owner, receiver)

    amount <- fromIntegral <$> L.getRandomInt (0, 100)
    let owner = getPub ownerKeyPair
        receiver = getPub receiverKeyPair
        currency = ENQ
    transaction <- signTransaction owner (getPriv ownerKeyPair) receiver amount currency    
    pure transaction

-- | Generate signed microblock
genMicroblock :: (Monad m, L.ERandom m) => StringHash -> [Transaction] -> m Microblock
genMicroblock hashofKeyBlock tx = do
    (KeyPair publisherPubKey publisherPrivKey)<- L.generateKeyPair
    microblock <- signMicroblock hashofKeyBlock tx publisherPubKey publisherPrivKey
    pure microblock

genRandMicroblock :: (Monad m, L.ERandom m) => KBlock -> m Microblock
genRandMicroblock kBlock = genMicroblock (toHash kBlock) =<< genNTransactions transactionsInMicroblock

-- | Generate indices with order
generateIndices :: (L.ERandom m, Monad m) => Ordering -> m [Int]
generateIndices order = do
    n <- case order of
        RandomOrder -> loopGenIndices [0 .. kBlockInBunch]
        InOrder     -> pure $ [0 .. kBlockInBunch]
    pure $ map fromIntegral n

-- loop: choose randomly one from the rest of list Integers
-- example:
-- [1,2,3,4,5] - 2
-- [1,3,4,5] - 4
-- [1,3,5] - 5
-- [1,3] - 1
-- [3] - 3
-- the result: [2,4,5,1,3]
loopGenIndices :: (Monad m, L.ERandom m, Eq a) => [a] -> m [a]
loopGenIndices numbers = do
    if (not $ null numbers)
        then do
            let maxIndex = length numbers - 1
            p <- L.getRandomInt (0, maxIndex)
            let result = numbers !! p
            -- choose next number from rest
            rest <- loopGenIndices $ delete result numbers
            pure (result : rest)
        else pure []
