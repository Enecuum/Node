{-# LANGUAGE DeriveGeneric #-}

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import qualified Crypto.Hash.SHA256                  as SHA
import qualified Data.ByteString.Char8               as BC
import           GHC.Generics
import           Service.Transaction.TransactionsDAG
import           Service.Types                       hiding (MicroblockV1)
import           Service.Types.PublicPrivateKeyPair
import           Service.Types.SerializeJSON         ()
import           System.Random

type HashOfMicroblock = BC.ByteString

data MicroblockV1 = MicroblockV1{
                  hashCurrentMicroblock  :: BC.ByteString, -- hashCurrentMicroblock
                  hashPreviousMicroblock :: BC.ByteString, -- hashPreviousMicroblock
                  trans                  :: [Transaction]}
                deriving (Eq, Generic, Ord, Show)


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
