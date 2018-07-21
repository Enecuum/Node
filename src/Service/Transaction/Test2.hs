{-# LANGUAGE DeriveGeneric #-}
module Service.Transaction.Test2 where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import qualified Crypto.Hash.SHA256                  as SHA
import qualified Data.ByteString.Char8               as BC
import           GHC.Generics
import           Service.Transaction.TransactionsDAG
import           Service.Types                       hiding (MicroblockV1)
--import           Service.Types.PublicPrivateKeyPair
import           Data.Maybe
import           Service.Types.SerializeJSON         ()
import           System.Random

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


aaa :: IO [(Int, Maybe Int)]
aaa = return [(1, Just 1), (3, Nothing)]



pick :: IO Int
pick = randomRIO (0, 1)

pickaaa :: IO (Int, Maybe Int)
pickaaa = do
  c <- aaa
  i <- pick
  return $ c !! i


readLinesUntilQuit :: IO [String]
readLinesUntilQuit = do
  line <- getLine
  if line /= "quit"
    then do
      -- recursive call, to loop
      restOfLines <- readLinesUntilQuit
      return (line : restOfLines)
    else return []


-- findaaa = do
--   p <- pickaaa
--   let second = \a -> isJust $ snd a
--   if (second p)
--     then do
--     rest <- findaaa
--     return (p:rest)
--     else return []
