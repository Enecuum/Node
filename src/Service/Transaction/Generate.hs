{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Service.Transaction.Generate where

import           Control.Monad                       (replicateM)
import           Control.Monad.State                 (StateT, evalStateT, get,
                                                      lift, put)
import           Service.Transaction.Storage         (getKeyBlockHash)
import           Service.Transaction.TransactionsDAG (genNTx)
import           Service.Types                       (HashOfKeyBlock,
                                                      KeyBlockInfoPoW (..),
                                                      Microblock (..))
import           Service.Types.PublicPrivateKeyPair  (KeyPair (..), generateNewRandomAnonymousKeyPair,
                                                      getSignature)


quantityOfTransactionInMicroblock :: Int
quantityOfTransactionInMicroblock = 10


quantityOfMicroblocksInKeyBlock :: Int
quantityOfMicroblocksInKeyBlock = 3


quantityOfPoAMiners :: Int
quantityOfPoAMiners = 3

hashOfgenesis :: HashOfKeyBlock
hashOfgenesis = "B1Vh7/LNOtWGd2+pBPAEAoLF9qJh9qj9agpSTRTNLSw="


genPoAMicroblock :: HashOfKeyBlock -> IO Microblock
genPoAMicroblock h = do
  tx <- genNTx quantityOfTransactionInMicroblock
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


genMicroblocksAndKeyBlock :: Int -> IO [(KeyBlockInfoPoW, [Microblock])]
genMicroblocksAndKeyBlock n = evalStateT (replicateM n generateMicroblocksAndKeyBlocks) (0, hashOfgenesis)
