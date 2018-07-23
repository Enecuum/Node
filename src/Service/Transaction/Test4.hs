{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Service.Transaction.Test4 where

import           Control.Monad                       (replicateM)
import           Control.Monad.State
import qualified Crypto.Hash.SHA256                  as SHA
import qualified Data.ByteString.Char8               as BC
import           Service.Transaction.Storage
import           Service.Transaction.TransactionsDAG
import           Service.Types
import           Service.Types.PublicPrivateKeyPair
import           Service.Types.SerializeJSON         ()
import           System.Random

-- data MicroblockV1 = MicroblockV1{
--                   hashCurrentMicroblock  :: ByteString, -- hashCurrentMicroblock
--                   hashPreviousMicroblock :: ByteString, -- hashPreviousMicroblock
--                   trans                  :: [Transaction]}
--                 deriving (Eq, Generic, Ord, Show)

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

{-
genPoWKeyBlocks :: StateT (Integer,HashOfKeyBlock) IO KeyBlockInfoPoW
genPoWKeyBlocks = do
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
   return key

generatePoWKeyBlock :: Int -> IO [KeyBlockInfoPoW]
generatePoWKeyBlock n = evalStateT (replicateM n genPoWKeyBlocks) (0, hashOfgenesis)
-}

hashOfgenesis :: HashOfKeyBlock
hashOfgenesis = "B1Vh7/LNOtWGd2+pBPAEAoLF9qJh9qj9agpSTRTNLSw="


genPoAMicroblock :: HashOfKeyBlock -> IO Microblock
genPoAMicroblock h = do
  tx <- genNNTx 5
  (KeyPair pubKey privateKey) <- generateNewRandomAnonymousKeyPair
  let aPublisher = pubKey
  aSign <- getSignature privateKey ("Secret message" :: String)
  keys <- replicateM 3 generateNewRandomAnonymousKeyPair
  let aTeamkeys = map (\(KeyPair p _) -> p) keys
  return Microblock{
  _keyBlock = h,
  _sign = aSign,
  _teamKeys = aTeamkeys,
  _publisher = aPublisher,
  _transactions = tx}

{-
generateMicroblocksAndKeyBlocks :: Int -> IO [(KeyBlockInfoPoW, [Microblock])]
generateMicroblocksAndKeyBlocks n = undefined
-}

generateMicroblocksAndKeyBlocks :: Int -> StateT (Integer,HashOfKeyBlock) IO (KeyBlockInfoPoW, [Microblock])
generateMicroblocksAndKeyBlocks n = do
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
   lift $ ((,) key) <$> replicateM n (genPoAMicroblock currentHash) 
