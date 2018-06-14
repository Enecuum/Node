{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Service.Transaction.Microblock where
import Service.Transaction.TransactionsDAG (genNNTx)
import qualified Data.ByteString.Char8 as BC
import qualified "cryptohash" Crypto.Hash.SHA1 as SHA1
import System.Random (randomRIO)
import Control.Monad (replicateM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, evalStateT, put, get)
-- import Service.Types (Time, Microblock(..),Transaction(..))
import Service.Types (Microblock(..),Transaction)
import Service.Types.PublicPrivateKeyPair (PublicKey(..)) -- Signature)
-- import Data.Aeson as A


type HashOfMicroblock = BC.ByteString

genNMicroBlocks :: Int -> IO [Microblock]
genNMicroBlocks n = evalStateT (replicateM n genMicroBlock) BC.empty


genMicroBlock :: StateT HashOfMicroblock IO Microblock
genMicroBlock = do
  aHashPreviousMicroblock <- get
  n <- lift $ randomRIO (3,4) --(40,128) -- from 40 to 128 transactions per block
  tx <- lift $ genNNTx n
  let aHashCurrentMicroblock = (SHA1.hash . BC.pack . show) tx
  put aHashCurrentMicroblock
  return (Microblock aHashCurrentMicroblock aHashPreviousMicroblock tx)


w1 :: IO ()
w1 = do
  let h01 = read "B0WvuBJEsQQ6RVqaEFaWoGXmW8sMG8xnBuxaCXJycRjyXN" :: PublicKey
  let h02 = "B0WvuBJEsQQ6RVqaEFaWoGXmW8sMG8xnBuxaCXJycRjyXN"
  putStrLn $ show $ (read (h02) :: PublicKey)
  putStrLn $ show $ (read (show h01) :: PublicKey)
  let tx1 = "WithTime {time = 48813.639349002, transaction = RegisterPublicKey {pubKey = B0WvuBJEsQQ6RVqaEFaWoGXmW8sMG8xnBuxaCXJycRjyXN, startWithBalance = 25}}"
  putStrLn $ show $ (read tx1 :: Transaction)
