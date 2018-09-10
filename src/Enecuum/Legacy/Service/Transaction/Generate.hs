{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Enecuum.Legacy.Service.Transaction.Generate where

import           Control.Monad                                      (replicateM)
import           Enecuum.Legacy.Service.Transaction.TransactionsDAG (genNTx)
import           Enecuum.Legacy.Service.Types                       (HashOfKeyBlock,
                                                                     KeyBlockInfoPoW (..),
                                                                     Microblock (..))
import           Enecuum.Legacy.Service.Types.PublicPrivateKeyPair  (KeyPair (..),
                                                                     generateNewRandomAnonymousKeyPair,
                                                                     getSignature)
import           Enecuum.Prelude

import           Enecuum.Legacy.Refact.Crypto                        ( calculateKeyBlockHash )
import           Enecuum.Legacy.Refact.Assets                        ( genesisKeyBlock )

quantityOfTransactionInMicroblock :: Int
quantityOfTransactionInMicroblock = 10

quantityOfMicroblocksInKeyBlock :: Int
quantityOfMicroblocksInKeyBlock = 3

quantityOfPoAMiners :: Int
quantityOfPoAMiners = 3

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
   let key = KeyBlockInfoPoW
        { _time = 0
        , _prev_hash = prev_hash
        , _number = aNumber
        , _nonce = 0
        , _solver = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
        , _type = 0
        }
       currentHash = calculateKeyBlockHash key
   put (aNumber + 1, currentHash)

  -- generate Microblocks for KeyBlock
   lift $ ((,) key) <$> replicateM quantityOfMicroblocksInKeyBlock (genPoAMicroblock currentHash)


genMicroblocksAndKeyBlock :: Int -> IO [(KeyBlockInfoPoW, [Microblock])]
genMicroblocksAndKeyBlock n = evalStateT (replicateM n generateMicroblocksAndKeyBlocks) (0, hashOfgenesis)
  where
    hashOfgenesis = calculateKeyBlockHash genesisKeyBlock
