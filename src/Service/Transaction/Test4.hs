{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Service.Transaction.Test4 where

import           Control.Monad.State
import qualified Crypto.Hash.SHA256                  as SHA
import qualified Data.ByteString.Char8               as BC
import           Service.Transaction.TransactionsDAG
import           Service.Types
-- import           Service.Types.PublicPrivateKeyPair
import           Service.Transaction.Storage
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


-- genPoWKeyBlocks :: StateT (Integer,HashOfKeyBlock) IO KeyBlockInfoPoW
genPoWKeyBlocks :: StateT HashOfKeyBlock IO KeyBlockInfoPoW
genPoWKeyBlocks = do
  -- (aNumber, prev_hash) <- get
  prev_hash <- get
  let aTime = 0
      aNonce = 0
      aType = 0
      aSolver = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
      key = KeyBlockInfoPoW{
        _time = aTime,
        _prev_hash = prev_hash,
        _number = 0, --aNumber,
        _nonce = aNonce,
        _solver = aSolver,
        _type = aType}
      currentHash = getKeyBlockHash key
  put currentHash
  -- put (number + 1, currentHash)
  return key


genPoWKeyBlocks2 :: StateT (Integer,HashOfKeyBlock) IO KeyBlockInfoPoW
genPoWKeyBlocks2 = do
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


-- [
--         {
--             "time": 0,
--             "nonce": 0,
--             "number": 0,
--             "type": 0,
--             "prev_hash": "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=",
--             "solver": "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
--         },
--         {
--             "time": 1532346450,
--             "nonce": 753983,
--             "number": 1,
--             "type": 0,
--             "prev_hash": "B1Vh7/LNOtWGd2+pBPAEAoLF9qJh9qj9agpSTRTNLSw=",
--             "solver": "OvS8LmmcMa4mtEWbifO5ZFkqT6AYRizzQ6mEobMMhz4="
--         },
--         {
--             "time": 1532346451,
--             "nonce": 986562,
--             "number": 2,
--             "type": 0,
--             "prev_hash": "AAABUnZe66jFjkvc1Nr6npBcZhZkauKL0+t+XgtqMRo=",
--             "solver": "OvS8LmmcMa4mtEWbifO5ZFkqT6AYRizzQ6mEobMMhz4="
--         },
--         {
--             "time": 1532346452,
--             "nonce": 392463,
--             "number": 3,
--             "type": 0,
--             "prev_hash": "AAABDXmD7qViron3Sl7l8SWWAi0KYEFEnPqvjMEJf4c=",
--             "solver": "OvS8LmmcMa4mtEWbifO5ZFkqT6AYRizzQ6mEobMMhz4="
--         },
--         {
--             "time": 1532346459,
--             "nonce": 1155359,
--             "number": 4,
--             "type": 0,
--             "prev_hash": "AAAAjZDpfGOLG38KrYn+ON6y69vZ5Y5NV9UQXBwHe8k=",
--             "solver": "OvS8LmmcMa4mtEWbifO5ZFkqT6AYRizzQ6mEobMMhz4="
--         }
--     ]
