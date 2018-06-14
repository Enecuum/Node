{-# LANGUAGE PackageImports #-}
module Service.Transaction.Balance ( getBalanceForKey, runLedger ) where

--import Data.Monoid (mconcat)
import Service.System.Directory (getTransactionFilePath,getLedgerFilePath)
import Service.Types.PublicPrivateKeyPair
import Service.Types
import Node.FileDB.FileDB (readHashMsgFromFile)
import qualified Data.ByteString.Char8 as BC hiding (map)
import qualified "rocksdb-haskell" Database.RocksDB as Rocks
import qualified Data.HashTable.IO as H
import qualified "cryptohash" Crypto.Hash.SHA1 as SHA1
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Default (def)
import Service.Transaction.Microblock (genNMicroBlocks)
import Control.Monad.Trans.Resource
import Service.System.Directory
import Data.Default (def)
import Data.Bits
--import Data.Hashable
import qualified Data.ByteString.Lazy as L
import Service.Types.SerializeInstances
import Control.Monad.Zip
import Control.Monad.IO.Class
import Data.Typeable
import Service.Transaction.Storage (DBdescriptor(..))


type HashOfMicroblock = BC.ByteString
--type NBalanceTable = H.BasicHashTable PublicKey Amount
type BalanceTable = H.BasicHashTable BC.ByteString Amount



-- functions for CLI
getBalanceForKey :: PublicKey -> IO Amount
getBalanceForKey key = runResourceT $ do
    pathL <- lift $ getLedgerFilePath
    (_, db) <- Rocks.openBracket pathL def{Rocks.createIfMissing=False}
    Just v  <- Rocks.get db Rocks.defaultReadOptions (rHash key)
    lift $ return (read (BC.unpack v) :: Amount)


-- for rocksdb
rHash key = SHA1.hash . BC.pack . show $ key
rValue value = BC.pack $ show value

-- for BalanceTable
htK key = BC.pack $ show key
unHtK key = read (BC.unpack key) :: PublicKey
unHtA key = read (BC.unpack key) :: Amount

updateBalanceTable :: BalanceTable -> Transaction -> IO ()
updateBalanceTable ht aTransaction = do
  case aTransaction of
    (WithSignature t _)        -> updateBalanceTable ht t
    (WithTime _ t)             -> updateBalanceTable ht t
    (RegisterPublicKey aKey aBalance) -> H.insert ht (htK aKey) aBalance
    (SendAmountFromKeyToKey fromKey toKey am) -> do v1 <- H.lookup ht $ htK fromKey
                                                    v2 <- H.lookup ht $ htK toKey
                                                    case (v1,v2) of
                                                      (Nothing, _)       -> do return ()
                                                      (_, Nothing)       -> do return ()
                                                      (Just balanceFrom, Just balanceTo) -> do H.insert ht (htK fromKey) (balanceFrom - am)
                                                                                               H.insert ht (htK toKey) (balanceTo + am)
    _ -> error "Unsupported type of transaction"


getTxsMicroblock (Microblock _ _ txs) = txs


getAllValues db = do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterFirst it
  Rocks.iterValues it


getBalanceOfKeys :: Rocks.DB -> [Transaction] -> IO BalanceTable
getBalanceOfKeys dbLedger tx = do
  let keys = concatMap getPubKeys tx
  let hashKeys = map htK keys
  balance  <- do
                let readKey k = (Rocks.get dbLedger Rocks.defaultReadOptions k)
                let toTuple k (Just b) = (,) k (unHtA b)
                mapM (\k -> liftM (toTuple k ) (readKey k)) hashKeys
  aBalanceTable <- H.fromList balance
  return aBalanceTable


getPubKeys :: Transaction -> [PublicKey]
getPubKeys (WithSignature t _) = getPubKeys t
getPubKeys (WithTime _ t) = getPubKeys t
getPubKeys (RegisterPublicKey aKey _) = [aKey]
getPubKeys (SendAmountFromKeyToKey fromKey toKey _) = [fromKey, toKey]


microblockIsExpected :: Microblock -> Bool
microblockIsExpected = undefined


runLedger :: DBdescriptor -> Microblock -> IO ()
runLedger (DBdescriptor dbTx dbMb dbLedger) m  = if (not $ microblockIsExpected m) then error "We are exepecting another microblock" else do
    let txs = getTxsMicroblock m
    ht      <- getBalanceOfKeys dbLedger txs
    mapM_ (updateBalanceTable ht) txs
-- Write to db atomically
    writeMicroblockDB dbMb m
    writeTransactionDB dbTx txs
    writeLedgerDB dbLedger ht


writeMicroblockDB :: Rocks.DB -> Microblock -> IO ()
writeMicroblockDB db m = do
  let key = rHash m
      val  = rValue m
  Rocks.write db def{Rocks.sync = True} [ Rocks.Put key val ]


writeTransactionDB :: Rocks.DB -> [Transaction] -> IO ()
writeTransactionDB db tx = do
  let txKeyValue = map (\t -> (rHash t, rValue t) ) tx
  Rocks.write db def{Rocks.sync = True} (map (\(k,v) -> Rocks.Put k v) txKeyValue)


writeLedgerDB ::  Rocks.DB -> BalanceTable -> IO ()
writeLedgerDB db bt = do
  ledgerKV <- H.toList bt
  let ledgerKeyValue = map (\(k,v)-> ((rHash . unHtK) k, rValue v)) ledgerKV
  Rocks.write db def{Rocks.sync = True} (map (\(k,v) -> Rocks.Put k v) ledgerKeyValue)
