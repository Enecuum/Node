{-# LANGUAGE PackageImports #-}

module Service.Transaction.Reward where


import qualified Data.ByteString.Char8 as BC
import Data.Serialize (Serialize, encode, decode)
import Data.Either
import qualified "rocksdb-haskell" Database.RocksDB as Rocks
import Data.Default (def)
import Data.Typeable

data Reward = RewardPoA | RewardPoW | RewardPoS


data Macroblock = Macroblock {
  keyBlock :: BC.ByteString,
  hashOfMicroblock :: [BC.ByteString]
                                 }

key = BC.pack "123"
hashes = [BC.pack "456", BC.pack "456"]
ma1 = Macroblock key hashes
-- writeToDB ma
-- ma <- readFromDB
-- hashes = read (hashes ma) :: [BC.ByteString]


  -- putStrLn $ BC.unpack result
  -- putStrLn "hello"

writeToDB ma = do
  let hashes2 = hashOfMicroblock ma
  let hashValue = encode $ show hashes2
  let path = "/tmp/haskell-rocksDB7"
  db <- Rocks.open path def{Rocks.createIfMissing=True}
  Rocks.write db def{Rocks.sync = True} [ Rocks.Put (keyBlock ma) hashValue]
  Just result <- Rocks.get db Rocks.defaultReadOptions key
  Rocks.close db
  -- let result2 = decode result :: Either String [BC.ByteString]
  -- putStrLn $ fromRight result2
  putStrLn ("type of result is: " ++ (show (typeOf result)))
  case (decode result) of
    Right a -> Prelude.putStrLn $ show $ (read a :: [BC.ByteString])
    Left a -> Prelude.putStrLn a


run = writeToDB ma1
