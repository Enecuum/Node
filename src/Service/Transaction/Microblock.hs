{-# LANGUAGE PackageImports #-}

module Service.Transaction.Microblock where
import Service.Transaction.TransactionsDAG (genNNTx)
import qualified Data.ByteString.Char8 as BC
import qualified "cryptohash" Crypto.Hash.SHA1 as SHA1
import System.Random (randomRIO)
import Control.Monad (replicateM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, evalStateT, put, get)
import Service.Types (Microblock(..))

type HashOfMicroblock = BC.ByteString

genNMicroBlocks :: Int -> IO [Microblock]
genNMicroBlocks n = evalStateT (replicateM n genMicroBlocks) BC.empty


genMicroBlocks :: StateT HashOfMicroblock IO Microblock
genMicroBlocks = do
  hashPreviousMicroblock <- get
  n <- lift $ randomRIO (3,4) --(40,128) -- from 40 to 128 transactions per block
  tx <- lift $ genNNTx n
  let hashCurrentMicroblock = (SHA1.hash . BC.pack . show) tx
  put hashCurrentMicroblock
  return (Microblock hashCurrentMicroblock hashPreviousMicroblock tx)
