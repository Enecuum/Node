{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Service.Transaction.Generate where

import           Control.Monad                       (replicateM)
import           Control.Monad.State
import qualified Crypto.Hash.SHA256                  as SHA
import qualified Data.ByteString.Char8               as BC
import           GHC.Generics
import           Service.Transaction.Storage
import           Service.Transaction.TransactionsDAG
import           Service.Types
import           Service.Types.PublicPrivateKeyPair
import           Service.Types.SerializeJSON         ()
import           System.Random


quantityOfTransactionInMicroblock :: Int
quantityOfTransactionInMicroblock = 2


quantityOfMicroblocksInKeyBlock :: Int
quantityOfMicroblocksInKeyBlock = 3


quantityOfPoAMiners :: Int
quantityOfPoAMiners = 3

hashOfgenesis :: HashOfKeyBlock
hashOfgenesis = "B1Vh7/LNOtWGd2+pBPAEAoLF9qJh9qj9agpSTRTNLSw="


genPoAMicroblock :: HashOfKeyBlock -> IO Microblock
genPoAMicroblock h = do
  tx <- genNNTx quantityOfTransactionInMicroblock
  (KeyPair pubKey privateKey) <- generateNewRandomAnonymousKeyPair
  let aPublisher = pubKey
  aSign <- getSignature privateKey ("Secret message" :: String)
  keys <- replicateM quantityOfPoAMiners generateNewRandomAnonymousKeyPair
  let aTeamkeys = map (\(KeyPair p _) -> p) keys
  return Microblock{
  _keyBlock = h,
  _sign = aSign,
  _teamKeys = aTeamkeys,
  _publisher = aPublisher,
  _transactions = tx}



generateMicroblocksAndKeyBlocks :: StateT (Integer,HashOfKeyBlock) IO (KeyBlockInfoPoW, [Microblock])
generateMicroblocksAndKeyBlocks = do
  -- generate KeyBlock
   (aNumber, prev_hash) <- get
   let aTime = 0
       aNonce = 0
       aType = 0
       aSolver = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
       key = KeyBlockInfoPoW{
         _time = aTime,
         _prev_hash = prev_hash,
         _number = aNumber,
         _nonce = aNonce,
         _solver = aSolver,
         _type = aType}
       currentHash = getKeyBlockHash key
   put (aNumber+1, currentHash)

  -- generate Microblocks for KeyBlock
   lift $ ((,) key) <$> replicateM quantityOfMicroblocksInKeyBlock (genPoAMicroblock currentHash)


genMicroblocksAndKeyBlocks :: Int -> IO [(KeyBlockInfoPoW, [Microblock])]
genMicroblocksAndKeyBlocks n = evalStateT (replicateM n generateMicroblocksAndKeyBlocks) (0, hashOfgenesis)


genNMicroBlocksV1 :: Int -> IO [MicroblockV1]
genNMicroBlocksV1 n = evalStateT (replicateM n genMicroBlockV1) BC.empty


genMicroBlockV1 :: StateT HashOfMicroblock IO MicroblockV1
genMicroBlockV1 = do
  aHashPreviousMicroblock <- get
  n <- lift $ randomRIO (3,4) --(40,128) -- from 40 to 128 transactions per block
  tx <- lift $ genNNTx n
  let aHashCurrentMicroblock = (SHA.hash . BC.pack . show) tx
  put aHashCurrentMicroblock
  return (MicroblockV1 aHashCurrentMicroblock aHashPreviousMicroblock tx)
