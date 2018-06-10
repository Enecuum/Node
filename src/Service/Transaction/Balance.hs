{-# LANGUAGE PackageImports #-}
module Service.Transaction.Balance ( countBalance ) where

--import Data.Monoid (mconcat)
import Service.System.Directory (getTransactionFilePath)
import Service.Types.PublicPrivateKeyPair
import Service.Types
import Node.FileDB.FileDB (readHashMsgFromFile)
import qualified "rocksdb-haskell" Database.RocksDB as Rocks
import  Data.ByteString.Char8 as BC hiding (map)

getBalance :: PublicKey -> [Transaction] -> Amount
getBalance key transactions = sum $ map getAmount transactions
  where
    getAmount (WithSignature t _) = getAmount t
    getAmount (WithTime _ t) = getAmount t
    getAmount (SendAmountFromKeyToKey from to a)
      | from == key  = a *(-1)
      | to == key    = a
      | otherwise    = 0
    getAmount _ = 0

countBalance :: PublicKey -> IO Amount
countBalance key = getBalance key <$> (readTransactions =<< getTransactionFilePath)
-- countBalance key = do
--   pathLedger <- getLedgerFilePath
--   dbh <- Rocks.open pathLedger def{Rocks.createIfMissing=True}
--   Just v  <- Rocks.get dbh Rocks.defaultReadOptions $ transformKey key
--   Rocks.close dbh
--   return ( read (BC.unpack v) :: Amount)

readTransactions :: String -> IO [Transaction]
readTransactions fileName = do
    mblocks <-  readHashMsgFromFile fileName
    let ts =  [trs | (Microblock _ _ trs) <- mblocks]
    return $ mconcat ts

transformKey key = BC.pack . show $ key
