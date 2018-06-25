{-# LANGUAGE PackageImports, FlexibleContexts, LambdaCase, DisambiguateRecordFields, DuplicateRecordFields #-}
module Service.Transaction.Balance
  ( getBalanceForKey,
    addMicroblockToDB,
    runLedger) where

import Service.Types.PublicPrivateKeyPair
import Service.Types hiding (MicroblockAPI(..))
import qualified Data.ByteString.Char8 as BC hiding (map)
import qualified "rocksdb-haskell" Database.RocksDB as Rocks
import qualified Data.HashTable.IO as H
import Control.Monad
import Service.Transaction.Storage (DBPoolDescriptor(..),DBPoolDescriptor(..),rHash, rValue, urValue, Macroblock(..), unA, getMicroBlockByHashDB)

import Data.Default (def)
import Data.Hashable
import Data.Pool
import Data.Serialize (decode, encode)
import Data.Either
import Control.Concurrent
import Service.InfoMsg (InfoMsg(..), LogingTag(..), MsgType(..))
import Node.Data.GlobalLoging
import Data.Maybe


instance Hashable PublicKey
type BalanceTable = H.BasicHashTable PublicKey Amount


getBalanceForKey :: DBPoolDescriptor -> PublicKey -> IO (Maybe Amount)
getBalanceForKey db key = do
    let fun = (\db -> Rocks.get db Rocks.defaultReadOptions (rValue key))
    val  <- withResource (poolLedger db) fun
    result <- case val of Nothing -> return Nothing --putStrLn "There is no such key"
                          Just v -> unA v
    return result


updateBalanceTable :: BalanceTable -> Transaction -> IO ()
updateBalanceTable ht (Transaction fromKey toKey am _ _ _ _) = do
  v1 <- H.lookup ht $ fromKey
  v2 <- H.lookup ht $ toKey
  case (v1,v2) of
    (Nothing, _)       -> do return ()
    (_, Nothing)       -> do return ()
    (Just balanceFrom, Just balanceTo) -> if (balanceFrom - am) > 0
                                          then do
                                                  H.insert ht fromKey (balanceFrom - am)
                                                  H.insert ht toKey (balanceTo + am)
                                          else do return ()


getTxsMicroblock :: Microblock -> [Transaction]
getTxsMicroblock (Microblock _ _ _ txs _) = txs


getBalanceOfKeys :: Pool Rocks.DB -> [Transaction] -> IO BalanceTable
getBalanceOfKeys db tx = do
  let hashKeys = concatMap getPubKeys tx
  let fun k = (\db -> Rocks.get db Rocks.defaultReadOptions (rValue k))
  let getBalanceByKey k = withResource db (fun k)
  let toTuple k b = (,) k b
  balance  <- mapM (\k -> liftM (toTuple k ) (getBalanceByKey k)) hashKeys
  --   balance (key Maybe(rValue Amount))

  -- initialize keys which does'not exist yet with initial balance = 100
  let keysWhichDoesNotExistYet = filter (\(k,v) -> v == Nothing) balance
  let initialBalance = map (\(k,v) ->  (rValue k, rValue (100 :: Amount))) keysWhichDoesNotExistYet
  let iBalance = map (\(key, val) -> Rocks.Put key val) initialBalance
  let fun2 = (\db -> Rocks.write db def{Rocks.sync = True} iBalance)
  withResource db fun2
  --
  let fun3 = \(k,v) -> case v of Nothing -> (k, 100 :: Amount)
                                 Just balance -> case (urValue balance :: Either String Amount) of
                                   Left _ -> (k, 0)
                                   Right b -> (k, b)

  let newBalance = map fun3 balance
  aBalanceTable <- H.fromList newBalance
  return aBalanceTable


runLedger :: DBPoolDescriptor -> Chan InfoMsg -> Microblock -> IO ()
runLedger db aInfoChan m = do
    let txs = getTxsMicroblock m
    ht      <- getBalanceOfKeys (poolLedger db) txs
    mapM_ (updateBalanceTable ht) txs
    writeLedgerDB (poolLedger db) aInfoChan ht


getPubKeys :: Transaction -> [PublicKey]
getPubKeys (Transaction fromKey toKey _ _ _ _ _) = [fromKey, toKey]


hashedMb hashesOfMicroblock = encode $ show hashesOfMicroblock

type HashOfMicroblock = BC.ByteString
checkMacroblock :: DBPoolDescriptor -> Chan InfoMsg -> BC.ByteString -> BC.ByteString -> IO (Bool, Bool, Bool, [HashOfMicroblock])
checkMacroblock db aInfoChan keyBlockHash blockHash = do
    let quantityMicroblocksInMacroblock = 2
    let fun = (\db -> Rocks.get db Rocks.defaultReadOptions keyBlockHash)
    v  <- withResource (poolMacroblock db) fun
    case v of Nothing -> do -- If Macroblock is not already in the table, than insert it into the table
                           let key = keyBlockHash
                               val = hashedMb [blockHash]
                           let fun = (\db -> Rocks.write db def{Rocks.sync = True} [ Rocks.Put key val ])
                           withResource (poolMacroblock db) fun
                           writeLog aInfoChan [BDTag] Info ("Write Macroblock " ++ show key ++ "to DB")
                           return (False, True, True, [])
              Just a -> do -- If Macroblock already in the table
                           -- let hashes = map (fromRight . decode) a -- (read a :: [BC.ByteString])
                           let hashes = case (decode a) of
                                         Right r -> (read r :: [BC.ByteString])
                                         Left _ -> error "Can not decode hashes of microblock "
                           if length hashes >= quantityMicroblocksInMacroblock
                           then return (True, True, False, [])
                           else do
                             -- let hashes = [BC.empty]
                             -- Check is Microblock already in Macroblock
                             let microblockIsAlreadyInMacroblock = blockHash `elem` hashes
                             writeLog aInfoChan [BDTag] Info ("Microblock is already in macroblock: " ++ show microblockIsAlreadyInMacroblock)
                             if microblockIsAlreadyInMacroblock
                               then return (False, False, True, hashes)  -- Microblock already in Macroblock - Nothing
                               else do -- write microblock (_ , True)
                                       -- add this Microblock to value of Macroblock
                                       let key = keyBlockHash
                                           val = hashedMb $ blockHash : hashes
                                       let fun = (\db -> Rocks.write db def{Rocks.sync = True} [ Rocks.Put key val ])
                                       withResource (poolMacroblock db) fun
                                       writeLog aInfoChan [BDTag] Info ("Write Microblock "  ++ show key ++ "to Macroblock table")
                                     -- Check quantity of microblocks, can we close Macroblock?
                                       if (length hashes >= (quantityMicroblocksInMacroblock - 1))
                                       then return (True, True, True, hashes)
                                       else return (False, True, True, hashes)



addMicroblockToDB :: DBPoolDescriptor -> Microblock -> Chan InfoMsg -> IO ()
addMicroblockToDB db m aInfoChan =  do
-- FIX: verify signature
    let txs = getTxsMicroblock m
    let microblockHash = rHash m
    writeLog aInfoChan [BDTag] Info ("New Microblock came" ++ show(microblockHash))

-- FIX: Write to db atomically
    (macroblockClosed, microblockNew, macroblockNew, microblockHashes ) <- checkMacroblock db aInfoChan (_keyBlock m) microblockHash
    writeLog aInfoChan [BDTag] Info ("Macroblock - New is " ++ show macroblockNew)
    writeLog aInfoChan [BDTag] Info ("Microblock - New is " ++ show microblockNew)
    writeLog aInfoChan [BDTag] Info ("Macroblock closed is " ++ show macroblockClosed)
    if (macroblockNew && microblockNew) then do
      writeMicroblockDB (poolMicroblock db) aInfoChan m
      writeTransactionDB (poolTransaction db) aInfoChan txs microblockHash
      if macroblockClosed then do
        -- get all microblocks (without last one) for macroblock
        mb <- mapM (\h -> getMicroBlockByHashDB db (Hash h))  microblockHashes
        let realMb = map fromJust (filter (isJust) mb) ++ [m]

        writeLog aInfoChan [BDTag] Info ("Will Write Ledger "  ++ show (length realMb))
        mapM (runLedger db aInfoChan) realMb
        return ()
        -- deleteMacroblockDB db aInfoChan (_keyBlock m) -- delete entry from Macroblock table
      else return ()
    else return ()


deleteMacroblockDB :: DBPoolDescriptor -> Chan InfoMsg -> BC.ByteString -> IO ()
deleteMacroblockDB db aInfoChan keyBlockHash = do
    let fun = (\db -> Rocks.delete db Rocks.defaultWriteOptions keyBlockHash)
    withResource (poolMacroblock db) fun
    writeLog aInfoChan [BDTag] Info ("Delete Macroblock "  ++ show keyBlockHash)


writeMicroblockDB :: Pool Rocks.DB -> Chan InfoMsg -> Microblock -> IO ()
writeMicroblockDB db aInfoChan m = do
  let key = rHash m
      val  = rValue m
  let fun = (\db -> Rocks.write db def{Rocks.sync = True} [ Rocks.Put key val ])
  withResource db fun
  writeLog aInfoChan [BDTag] Info ("Write Microblock "  ++ show key ++ "to Microblock table")


writeTransactionDB :: Pool Rocks.DB -> Chan InfoMsg -> [Transaction] -> BC.ByteString -> IO ()
writeTransactionDB dbTransaction aInfoChan tx hashOfMicroblock = do
  let txInfo = \tx1 num -> TransactionInfo tx1 hashOfMicroblock num
  let txKeyValue = map (\(t,n) -> (rHash t, rValue (txInfo t n)) ) (zip tx [1..])
  let fun = (\db -> Rocks.write db def{Rocks.sync = True} (map (\(k,v) -> Rocks.Put k v) txKeyValue))
  withResource dbTransaction fun
  writeLog aInfoChan [BDTag] Info ("Write Transactions to Transaction table")


writeLedgerDB ::  Pool Rocks.DB -> Chan InfoMsg -> BalanceTable -> IO ()
writeLedgerDB dbLedger aInfoChan bt = do
  ledgerKV <- H.toList bt
  let ledgerKeyValue = map (\(k,v)-> (rValue k, rValue v)) ledgerKV
  let fun = (\db -> Rocks.write db def{Rocks.sync = True} (map (\(k,v) -> Rocks.Put k v) ledgerKeyValue))
  withResource dbLedger fun
  writeLog aInfoChan [BDTag] Info ("Write Ledger "  ++ show bt)
