{-# LANGUAGE PackageImports, FlexibleContexts, LambdaCase #-}
module Service.Transaction.Balance
  ( getBalanceForKey,
    addMicroblockToDB,
    runLedger) where

import Service.Types.PublicPrivateKeyPair
import Service.Types
import qualified Data.ByteString.Char8 as BC hiding (map)
import qualified "rocksdb-haskell" Database.RocksDB as Rocks
import qualified Data.HashTable.IO as H
import Control.Monad
import Service.Transaction.Storage (DBPoolDescriptor(..),DBPoolDescriptor(..),rHash, rValue, urValue, unHtK, unHtA, Macroblock(..))

import Data.Default (def)
import Data.Hashable
import Data.Pool
import Data.Serialize (decode, encode)
import Data.Either

instance Hashable PublicKey
type BalanceTable = H.BasicHashTable PublicKey Amount


getBalanceForKey :: DBPoolDescriptor -> PublicKey -> IO Amount
getBalanceForKey db key = do
    let fun = (\db -> Rocks.get db Rocks.defaultReadOptions (rValue key))
    Just v  <- withResource (poolLedger db) fun
    return (unHtA v)


updateBalanceTable :: BalanceTable -> Transaction -> IO ()
updateBalanceTable ht (Transaction fromKey toKey am _ _ _) = do
  v1 <- H.lookup ht $ fromKey
  v2 <- H.lookup ht $ toKey
  case (v1,v2) of
    (Nothing, _)       -> do return ()
    (_, Nothing)       -> do return ()
    (Just balanceFrom, Just balanceTo) -> do H.insert ht fromKey (balanceFrom - am)
                                             H.insert ht toKey (balanceTo + am)



getTxsMicroblock :: Microblock -> [Transaction]
getTxsMicroblock (Microblock _ _ _ _ txs _) = txs


getBalanceOfKeys :: Pool Rocks.DB -> [Transaction] -> IO BalanceTable
-- getBalanceOfKeys = undefined
getBalanceOfKeys db tx = do
  let hashKeys = concatMap getPubKeys tx
  let fun k = (\db -> Rocks.get db Rocks.defaultReadOptions (rValue k))
  let getBalanceByKey k = withResource db (fun k)
  let toTuple k (Just b) = (,) k (unHtA b)
  balance  <- mapM (\k -> liftM (toTuple k ) (getBalanceByKey k)) hashKeys
  aBalanceTable <- H.fromList balance
  return aBalanceTable


getPubKeys :: Transaction -> [PublicKey]
getPubKeys (Transaction fromKey toKey _ _ _ _) = [fromKey, toKey]



runLedger :: DBPoolDescriptor -> Microblock -> IO ()
runLedger db m = do
    let txs = getTxsMicroblock m
    ht      <- getBalanceOfKeys (poolLedger db) txs
    mapM_ (updateBalanceTable ht) txs
    writeLedgerDB (poolLedger db) ht


hashedMb hashesOfMicroblock = encode $ show hashesOfMicroblock

checkMacroblock :: DBPoolDescriptor -> BC.ByteString -> BC.ByteString -> IO (Bool, Bool)
checkMacroblock db keyBlockHash blockHash = do --undefined
    let fun = (\db -> Rocks.get db Rocks.defaultReadOptions keyBlockHash)
    v  <- withResource (poolMacroblock db) fun
    case v of Nothing -> do -- If Macroblock is not already in the table, than insert it into the table
                           let key = keyBlockHash
                               val = hashedMb [blockHash]
                           let fun = (\db -> Rocks.write db def{Rocks.sync = True} [ Rocks.Put key val ])
                           withResource (poolMacroblock db) fun
                           return (False, True)
              Just a -> do -- If Macroblock already in the table
                           -- let hashes = map (fromRight . decode) a -- (read a :: [BC.ByteString])
                           let hashes = case (decode a) of
                                         Right r -> (read r :: [BC.ByteString])
                                         Left _ -> [BC.empty]
                           -- let hashes = [BC.empty]
                           -- Check is Microblock already in Macroblock
                           let microblockIsAlreadyInMacroblock = blockHash `elem` hashes
                           if microblockIsAlreadyInMacroblock
                             then return (False, False)  -- Microblock already in Macroblock - Nothing
                             else do -- write microblock (_ , True)
                                     -- add this Microblock to value of Macroblock
                                     let key = keyBlockHash
                                         val = hashedMb (blockHash : hashes)
                                     let fun = (\db -> Rocks.write db def{Rocks.sync = True} [ Rocks.Put key val ])
                                     withResource (poolMacroblock db) fun
                                     -- Check quantity of microblocks, can we close Macroblock?
                                     if (length hashes >= 63)
                                       then return (True, True)
                                     else return (False, True)



addMicroblockToDB :: DBPoolDescriptor -> Microblock -> IO ()
addMicroblockToDB db m  =  do
-- FIX: verify signature
    let txs = getTxsMicroblock m

-- FIX: Write to db atomically
    (macroblockClosed, microblockNew) <- checkMacroblock db (_keyBlock m) (rHash m)
    if microblockNew then do
      writeMicroblockDB (poolMicroblock db) m
      writeTransactionDB (poolTransaction db) txs
      if macroblockClosed then do
        runLedger db m
        deleteMacroblockDB db (_keyBlock m) -- delete entry from Macroblock table
      else return ()
    else return ()


deleteMacroblockDB :: DBPoolDescriptor -> BC.ByteString -> IO ()
deleteMacroblockDB db keyBlockHash = do --undefined
    let fun = (\db -> Rocks.delete db Rocks.defaultWriteOptions keyBlockHash)
    withResource (poolMacroblock db) fun


writeMicroblockDB :: Pool Rocks.DB -> Microblock -> IO ()
writeMicroblockDB db m = do
  let key = rHash m
      val  = rValue m
  let fun = (\db -> Rocks.write db def{Rocks.sync = True} [ Rocks.Put key val ])
  withResource db fun


writeTransactionDB :: Pool Rocks.DB -> [Transaction] -> IO ()
writeTransactionDB dbTransaction tx = do
  let txKeyValue = map (\t -> (rHash t, rValue t) ) tx
  let fun = (\db -> Rocks.write db def{Rocks.sync = True} (map (\(k,v) -> Rocks.Put k v) txKeyValue))
  withResource dbTransaction fun


writeLedgerDB ::  Pool Rocks.DB -> BalanceTable -> IO ()
writeLedgerDB dbLedger bt = do
  ledgerKV <- H.toList bt
  let ledgerKeyValue = map (\(k,v)-> (rValue k, rValue v)) ledgerKV
  let fun = (\db -> Rocks.write db def{Rocks.sync = True} (map (\(k,v) -> Rocks.Put k v) ledgerKeyValue))
  withResource dbLedger fun
