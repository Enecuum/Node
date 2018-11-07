{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}

module Enecuum.Assets.Blockchain.Generation where

import           Data.HGraph.StringHashable       (StringHash (..), toHash)
import           Data.List                        (delete)
import           Enecuum.Assets.Blockchain.Wallet
import           Enecuum.Blockchain.Domain
import qualified Enecuum.Blockchain.Lens          as Lens
import qualified Enecuum.Domain                   as D
import qualified Enecuum.Language                 as L
import           Enecuum.Prelude                  hiding (Ordering)

-- | Order for key blocks
data Ordering = InOrder | RandomOrder
    deriving (Show, Eq, Generic, ToJSON, FromJSON)


-- | WalletSource for transaction generation (Hardcoded - for demo purpose, Generated - for production)
data WalletSource = Generated | Hardcoded
    deriving (Show)

kBlockInBunch :: D.BlockNumber
kBlockInBunch = 1

transactionsInMicroblock :: Int
transactionsInMicroblock = 3

generateNKBlocks :: (L.ERandom m, Monad m) => D.BlockNumber -> m (StringHash, [KBlock])
generateNKBlocks = generateKBlocks genesisHash

generateNKBlocksWithOrder :: D.BlockNumber -> Ordering -> L.ERandomL (StringHash, [KBlock])
generateNKBlocksWithOrder = createKBlocks genesisHash

-- Generate bunch of key blocks (randomly or in order)
createKBlocks :: (Monad m, L.ERandom m) => StringHash -> D.BlockNumber -> Ordering -> m (StringHash, [KBlock])
createKBlocks prevKBlockHash from order = do
    (lastHash, kBlockBunch) <- generateKBlocks prevKBlockHash from
    kBlockIndices           <- generateIndices order
    let kBlocks = map (kBlockBunch !!) kBlockIndices
    pure (lastHash, kBlocks)

-- Generate bunch of key blocks (in order)
generateKBlocks :: Monad m => StringHash -> D.BlockNumber -> m (StringHash, [KBlock])
generateKBlocks prevHash from = do
    blocks <- loopGenKBlock prevHash from (from + kBlockInBunch)
    case blocks of
        [] -> pure (prevHash, [])
        _  -> pure (toHash $ last blocks, blocks)

-- loop - state substitute : create new Kblock using hash of previous
loopGenKBlock :: Monad m => StringHash -> D.BlockNumber -> D.BlockNumber -> m [KBlock]
loopGenKBlock prevHash from to = do
    let kblock      = genKBlock prevHash from
        newPrevHash = toHash kblock
    if from < to
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

genKBlock :: StringHash -> D.BlockNumber -> KBlock
genKBlock prevHash i = KBlock
    { _prevHash = prevHash
    , _number   = i
    , _nonce    = i
    , _solver   = toHash (i + 3)
    , _time     = i
    }

genNTransactions :: (L.ERandom m, Monad m) => Int -> m [Transaction]
genNTransactions k = replicateM k $ genTransaction Hardcoded

-- | Generate signed transaction
genTransaction :: (Monad m, L.ERandom m) => WalletSource -> m Transaction
genTransaction isFromRange = do
    (ownerKeyPair, receiverKeyPair) <- case isFromRange of
        Hardcoded -> do
            let quantityOfWallets = length hardcodedWallets
            ownerIndex <- L.getRandomInt (0, quantityOfWallets - 1)
            let owner = hardcodedWallets !! ownerIndex
            let rest = delete owner hardcodedWallets
            receiverIndex <- L.getRandomInt (0, quantityOfWallets - 2)
            let receiver = rest !! receiverIndex
            pure (owner, receiver)
        Generated -> do
            owner <- L.evalCoreCrypto L.generateKeyPair
            receiver <- L.evalCoreCrypto L.generateKeyPair
            pure (owner, receiver)

    amount <- fromIntegral <$> L.getRandomInt (0, 100)
    let owner = getPub ownerKeyPair
        receiver = getPub receiverKeyPair
        currency = ENQ
    uuid <- L.nextUUID
    signTransaction owner (getPriv ownerKeyPair) receiver amount currency uuid

-- | Generate signed microblock
genMicroblock :: (Monad m, L.ERandom m) => KBlock -> [Transaction] -> m Microblock
genMicroblock kBlock tx = do
    let hashofKeyBlock = toHash kBlock
    KeyPair publisherPubKey publisherPrivKey <- L.evalCoreCrypto L.generateKeyPair
    signMicroblock hashofKeyBlock tx publisherPubKey publisherPrivKey

genRandMicroblock :: (Monad m, L.ERandom m) => KBlock -> m Microblock
genRandMicroblock kBlock = genMicroblock kBlock =<< genNTransactions transactionsInMicroblock

-- | Generate indices with order
generateIndices :: (L.ERandom m, Monad m) => Ordering -> m [Int]
generateIndices order = do
    n <- case order of
        RandomOrder -> loopGenIndices [0 .. kBlockInBunch]
        InOrder     -> pure [0 .. kBlockInBunch]
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
loopGenIndices [] = pure []
loopGenIndices numbers = do
    let maxIndex = length numbers - 1
    p <- L.getRandomInt (0, maxIndex)
    let result = numbers !! p
    -- choose next number from rest
    rest <- loopGenIndices $ delete result numbers
    pure (result : rest)

-- | Generate bogus transaction
generateBogusSignedTransaction :: (Monad m, L.ERandom m) => m Transaction
generateBogusSignedTransaction = do
    Transaction {..} <- genTransaction Generated
    let genTxSign fakeOwnerPrivateKey = signTransaction _owner fakeOwnerPrivateKey _receiver (_amount + 100)  _currency _uuid
    generateBogusSignedSomething genTxSign

-- | Generate bogus signature with function for something
generateBogusSignedSomething :: (Monad m, L.ERandom m, Lens.HasSignature s b) =>
    (PrivateKey -> m s) -> m s
generateBogusSignedSomething genFunction = do
    fakeOwner <- L.evalCoreCrypto L.generateKeyPair
    let fakeOwnerPrivateKey = getPriv fakeOwner
    genFunction fakeOwnerPrivateKey

-- | Generate bogus microblock
generateBogusSignedMicroblock :: (Monad m, L.ERandom m) => KBlock -> [Transaction] -> m Microblock
generateBogusSignedMicroblock kBlock tx = do
    Microblock {..} <- genMicroblock kBlock tx
    let genMbSign = signMicroblock _keyBlock _transactions _publisher
    generateBogusSignedSomething genMbSign
