{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE RecordWildCards       #-}

module Enecuum.Assets.Nodes.Generation where

import           Data.HGraph.StringHashable  (StringHash (..), toHash)
import           Data.List                   (delete)
import           Enecuum.Assets.Nodes.Wallet
import           Enecuum.Blockchain.Domain
import qualified Enecuum.Blockchain.Lens     as Lens
import qualified Enecuum.Language            as L
import           Enecuum.Prelude             hiding (Ordering)

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

-- | Generate bogus signed transaction
-- generateBogusSignedTransaction :: (Monad m, L.ERandom m) => m Transaction
-- generateBogusSignedTransaction = do
--     fakeOwner <- L.evalCoreCrypto $ L.generateKeyPair
--     tx@(Transaction {..}) <- genTransaction Off
--     fakeTransaction <- signTransaction _owner (getPriv fakeOwner) _receiver (_amount + 100)  _currency
--     let fakeSignature = fakeTransaction ^. Lens.signature
--     pure Transaction
--             { _owner     = _owner
--             , _receiver  = _receiver
--             , _amount    = _amount
--             , _currency  = _currency
--             , _signature = fakeSignature
--             }

generateBogusSignedTransaction :: (Monad m, L.ERandom m) => m Transaction
generateBogusSignedTransaction = do
    tx@(Transaction {..}) <- genTransaction Off
    let genTx fakeOwnerPrivateKey = signTransaction _owner fakeOwnerPrivateKey _receiver (_amount + 100)  _currency
    fakeSignature <- generateBogusSignedSomething genTx
    pure Transaction
            { _owner     = _owner
            , _receiver  = _receiver
            , _amount    = _amount
            , _currency  = _currency
            , _signature = fakeSignature
            }

generateBogusSignedSomething genFunction = do 
    fakeOwner <- L.evalCoreCrypto $ L.generateKeyPair
    let fakeOwnerPrivateKey = getPriv fakeOwner
    something <- genFunction fakeOwnerPrivateKey
    pure $ something ^. Lens.signature

-- generateBogusSignedMicroblock :: (Monad m, L.ERandom m) => m Transaction
-- generateBogusSignedMicroblock = do
--     undefined

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
            owner <- L.evalCoreCrypto $ L.generateKeyPair
            receiver <- L.evalCoreCrypto $ L.generateKeyPair
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
    (KeyPair publisherPubKey publisherPrivKey)<- L.evalCoreCrypto $ L.generateKeyPair
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
