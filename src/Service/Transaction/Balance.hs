{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PackageImports           #-}

module Service.Transaction.Balance
  ( getBalanceForKey,
    addMicroblockToDB,
    addMacroblockToDB,
    runLedger,
    writeMacroblockToDB
    ) where

import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types                      (parseMaybe)
import qualified Data.ByteString.Char8                 as BC
import           Data.Default                          (def)
import           Data.Either
import           Data.Hashable
import qualified Data.HashTable.IO                     as H
import           Data.List                             (sortBy)
import           Data.Maybe
import           Data.Ord                              (comparing)
import           Data.Pool
import qualified Data.Serialize                        as S (Serialize, decode,
                                                             encode)
import qualified "rocksdb-haskell" Database.RocksDB    as Rocks
import           Node.Data.GlobalLoging
import           Service.InfoMsg                       (InfoMsg (..),
                                                        LogingTag (..),
                                                        MsgType (..))
import           Service.Transaction.Storage
import           Service.Types                         hiding
                                                        (MicroblockAPI (..))
import           Service.Types.PublicPrivateKeyPair

instance Hashable PublicKey
type BalanceTable = H.BasicHashTable PublicKey Amount


getBalanceForKey :: DBPoolDescriptor -> PublicKey -> IO (Maybe Amount)
getBalanceForKey db key = do
    let fun = (\db -> Rocks.get db Rocks.defaultReadOptions (rValue key))
    val  <- withResource (poolLedger db) fun
    result <- case val of Nothing -> return Nothing --putStrLn "There is no such key"
                          Just v  -> unA v
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

-- getTxsMicroblock = undefined
getTxsMicroblock :: DBPoolDescriptor -> Microblock -> IO [Transaction]
getTxsMicroblock db (Microblock _ _ _ txHashes _) = do
  let fun kTransactionHash = (\db -> Rocks.get db Rocks.defaultReadOptions kTransactionHash)
  maybeTxUntiped  <- mapM (\k -> withResource (poolTransaction db) (fun k)) txHashes
  let txDoesNotExist = filter (\t -> t /= Nothing) maybeTxUntiped
  if null txDoesNotExist
    then error "Some of transactions can not be found"
    else do
         let txUntiped = map fromJust (filter (isJust) maybeTxUntiped)
             extract t = case S.decode t :: Either String TransactionInfo of
                            Left _  -> error "Can not decode TransactionInfo"
                            Right r -> r
             tx = map (_tx . extract) txUntiped
         return tx


getBalanceOfKeys :: Pool Rocks.DB -> [Transaction] -> IO BalanceTable
getBalanceOfKeys db tx = do
  let hashKeys = concatMap getPubKeys tx
  let fun k = (\db -> Rocks.get db Rocks.defaultReadOptions (rValue k))
  let getBalanceByKey k = withResource db (fun k)
  let toTuple k b = (,) k b
  balance  <- mapM (\k -> liftM (toTuple k ) (getBalanceByKey k)) hashKeys
  --   balance (key Maybe(rValue Amount))

  -- initialize keys which does'not exist yet with initial balance = 100
  let keysWhichDoesNotExistYet = filter (\(_,v) -> v == Nothing) balance
  let initialBalance = map (\(k,_) ->  (rValue k, rValue (100 :: Amount))) keysWhichDoesNotExistYet
  let iBalance = map (\(key, val) -> Rocks.Put key val) initialBalance
  let fun2 = (\db -> Rocks.write db def{Rocks.sync = True} iBalance)
  withResource db fun2
  --
  let fun3 = \(k,v) -> case v of Nothing -> (k, 100 :: Amount)
                                 Just balance -> case (urValue balance :: Either String Amount) of
                                   Left _  -> (k, 0)
                                   Right b -> (k, b)

  let newBalance = map fun3 balance
  aBalanceTable <- H.fromList newBalance
  return aBalanceTable


runLedger :: DBPoolDescriptor -> InChan InfoMsg -> Microblock -> IO ()
runLedger db aInfoChan m = do
    txs <- getTxsMicroblock db m
    ht      <- getBalanceOfKeys (poolLedger db) txs
    mapM_ (updateBalanceTable ht) txs
    writeLedgerDB (poolLedger db) aInfoChan ht


getPubKeys :: Transaction -> [PublicKey]
getPubKeys (Transaction fromKey toKey _ _ _ _ _) = [fromKey, toKey]


type HashOfMicroblock = BC.ByteString
checkMacroblock :: DBPoolDescriptor -> InChan InfoMsg -> BC.ByteString -> BC.ByteString -> IO (Bool, Bool, Bool, Maybe Macroblock)
checkMacroblock db aInfoChan keyBlockHash blockHash = do
    let quantityMicroblocksInMacroblock = 2
    let fun = (\db -> Rocks.get db Rocks.defaultReadOptions keyBlockHash)
    v  <- withResource (poolMacroblock db) fun

    case v of
      Nothing -> do -- If Macroblock is not already in the table, than insert it into the table
                    let aMacroBlock = dummyMacroblock {_mblocks = [blockHash]}
                    writeMacroblockToDB db aInfoChan keyBlockHash aMacroBlock
                    return (False, True, True, Nothing)
      Just a -> -- If Macroblock already in the table
        case urValue a :: Either String Macroblock of
          Left _  -> error "Can not decode Macroblock"
          Right bdMacroblock -> do
                   let hashes = _mblocks bdMacroblock
                   if length hashes >= quantityMicroblocksInMacroblock
                   then return (True, True, False, Just bdMacroblock)
                   else do
                     -- Check is Microblock already in Macroblock
                     let microblockIsAlreadyInMacroblock = blockHash `elem` hashes
                     writeLog aInfoChan [BDTag] Info ("Microblock is already in macroblock: " ++ show microblockIsAlreadyInMacroblock)
                     if microblockIsAlreadyInMacroblock
                       then return (False, False, True, Nothing)  -- Microblock already in Macroblock - Nothing
                       else do -- add this Microblock to the value of Macroblock
                               let aMacroBlock = bdMacroblock {  _mblocks = blockHash : hashes}
                               writeMacroblockToDB db aInfoChan keyBlockHash aMacroBlock
                             -- Check quantity of microblocks, can we close Macroblock?
                               if (length hashes >= (quantityMicroblocksInMacroblock - 1))
                                       then return (True, True, True, Just bdMacroblock)
                                       else return (False, True, True, Nothing)


addMicroblockToDB :: DBPoolDescriptor -> Microblock -> InChan InfoMsg -> IO ()
addMicroblockToDB db m aInfoChan =  do
-- FIX: verify signature
    txs <- getTxsMicroblock db m
    let microblockHash = rHash m
    writeLog aInfoChan [BDTag] Info ("New Microblock came" ++ show(microblockHash))

-- FIX: Write to db atomically
    (macroblockClosed, microblockNew, macroblockNew, macroblock ) <- checkMacroblock db aInfoChan (_keyBlock m) microblockHash
    writeLog aInfoChan [BDTag] Info ("Macroblock - New is " ++ show macroblockNew)
    writeLog aInfoChan [BDTag] Info ("Microblock - New is " ++ show microblockNew)
    writeLog aInfoChan [BDTag] Info ("Macroblock closed is " ++ show macroblockClosed)
    if (macroblockNew && microblockNew) then do
      writeMicroblockDB (poolMicroblock db) aInfoChan m
      writeTransactionDB (poolTransaction db) aInfoChan txs microblockHash

      if macroblockClosed then do
        -- get all microblocks (without the last added) for macroblock
        let aMacroblock = fromJust macroblock
        let microblockHashes = tail $ _mblocks aMacroblock
        mb <- mapM (\h -> getMicroBlockByHashDB db (Hash h))  microblockHashes
        let realMb = sortBy (comparing _sign) $ map fromJust (filter (isJust) mb) ++ [m]

        writeLog aInfoChan [BDTag] Info ("Start calculate Ledger "  ++ show (length realMb))
        mapM_ (runLedger db aInfoChan) realMb
        writeMacroblockToDB db aInfoChan (_keyBlock m) (aMacroblock {_reward = 10})
      else return ()
    else return ()


writeMacroblockToDB :: DBPoolDescriptor -> InChan InfoMsg -> BC.ByteString -> Macroblock -> IO ()
writeMacroblockToDB desc aInfoChan keyBlock aMacroblock = do
  let key = keyBlock
      val = rValue aMacroblock
      fun = (\db -> Rocks.write db def{Rocks.sync = True} [ Rocks.Put key val ])
  withResource (poolMacroblock desc) fun
  writeLog aInfoChan [BDTag] Info ("Write Macroblock " ++ show key ++ "to DB")


writeMicroblockDB :: Pool Rocks.DB -> InChan InfoMsg -> Microblock -> IO ()
writeMicroblockDB db aInfoChan m = do
  let key = rHash m
      val  = rValue m
  let fun = (\db -> Rocks.write db def{Rocks.sync = True} [ Rocks.Put key val ])
  withResource db fun
  writeLog aInfoChan [BDTag] Info ("Write Microblock "  ++ show key ++ "to Microblock table")


writeTransactionDB :: Pool Rocks.DB -> InChan InfoMsg -> [Transaction] -> BC.ByteString -> IO ()
writeTransactionDB dbTransaction aInfoChan tx hashOfMicroblock = do
  let txInfo = \tx1 num -> TransactionInfo tx1 hashOfMicroblock num
  let txKeyValue = map (\(t,n) -> (rHash t, rValue (txInfo t n)) ) (zip tx [1..])
  let fun = (\db -> Rocks.write db def{Rocks.sync = True} (map (\(k,v) -> Rocks.Put k v) txKeyValue))
  withResource dbTransaction fun
  writeLog aInfoChan [BDTag] Info ("Write Transactions to Transaction table")


writeLedgerDB ::  Pool Rocks.DB -> InChan InfoMsg -> BalanceTable -> IO ()
writeLedgerDB dbLedger aInfoChan bt = do
  ledgerKV <- H.toList bt
  let ledgerKeyValue = map (\(k,v)-> (rValue k, rValue v)) ledgerKV
  let fun = (\db -> Rocks.write db def{Rocks.sync = True} (map (\(k,v) -> Rocks.Put k v) ledgerKeyValue))
  withResource dbLedger fun
  writeLog aInfoChan [BDTag] Info ("Write Ledger "  ++ show bt)



addMacroblockToDB :: DBPoolDescriptor -> Value -> InChan InfoMsg -> IO ()
addMacroblockToDB db aValue aInfoChan = do
  -- writeLog aInfoChan [BDTag] Info ("A.Value is " ++ show aValue)
  let (Object v) = aValue
  let keyBlock = case parseMaybe extractKeyBlock v of
        Nothing     -> error "Can not parse KeyBlock" --Data.Map.empty
        Just kBlock -> kBlock :: String --Map T.Text Value
        where extractKeyBlock = \info -> info .: "msg"
                                 >>=
                                \msg -> msg .: "verb"
  if (keyBlock /= "kblock")
    then return ()
  else do
    let keyBlockInfoObject = case parseMaybe extractKeyBlockInfo v of
                                  Nothing     -> error "Can not parse KeyBlockInfo" --Data.Map.empty
                                  Just kBlock -> kBlock :: KeyBlockInfo --Map T.Text Value
                             where extractKeyBlockInfo = \info -> info .: "msg"
                                                         >>=
                                                         \msg -> msg .: "body"

    writeLog aInfoChan [BDTag] Info (show keyBlockInfoObject)
    let keyBlockHash = rHash keyBlockInfoObject

    -- get data about macroblock from DB
    let fun = (\db -> Rocks.get db Rocks.defaultReadOptions keyBlockHash)
    val  <- withResource (poolMacroblock db) fun
    aMacroblock <- case val of Nothing -> return dummyMacroblock
                               Just v  -> case urValue v :: Either String Macroblock of
                                 Left _  -> error "Can not decode Macroblock"
                                 Right r -> return r

    -- fill data for key block
    let prevHash = read (prev_hash keyBlockInfoObject) :: BC.ByteString
        aSolver = solver keyBlockInfoObject
        atimeK = time keyBlockInfoObject
        aNumberK = number keyBlockInfoObject
        aNonce = nonce keyBlockInfoObject
    let fMacroblock = aMacroblock { _prevBlock = prevHash, _difficulty = 20, _solver = aSolver, _timeK = atimeK, _numberK = aNumberK, _nonce = aNonce }

    writeMacroblockToDB db aInfoChan keyBlockHash fMacroblock


dummyMacroblock :: Macroblock
dummyMacroblock = Macroblock { _prevBlock = "", _difficulty = 0, _height = 0, _solver = aSolver, _reward = 0, _mblocks = [], _timeK = 0, _numberK = 0, _nonce = 0}
  where aSolver = read "" :: PublicKey
