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
import           Data.Hashable
import qualified Data.HashTable.IO                     as H
import           Data.List                             (sortBy)
import           Data.Maybe
import           Data.Ord                              (comparing)
import           Data.Pool
import qualified Data.Serialize                        as S (Serialize, decode,
                                                             encode)
import           Data.Typeable
import qualified "rocksdb-haskell" Database.RocksDB    as Rocks
import           Node.Data.GlobalLoging
import           Service.InfoMsg                       (InfoMsg (..),
                                                        LogingTag (..),
                                                        MsgType (..))
import           Service.Transaction.Storage
import           Service.Types
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




getBalanceOfKeys :: Pool Rocks.DB -> [Transaction] -> IO BalanceTable
getBalanceOfKeys db tx = do
  let hashKeys = concatMap getPubKeys tx
      fun k = (\db -> Rocks.get db Rocks.defaultReadOptions (rValue k))
      getBalanceByKey k = withResource db (fun k)
      toTuple k b = (,) k b
  balance  <- mapM (\k -> liftM (toTuple k ) (getBalanceByKey k)) hashKeys
  --   balance (key Maybe(rValue Amount))

  -- initialize keys which does'not exist yet with initial balance = 100
  let keysWhichDoesNotExistYet = filter (\(_,v) -> v == Nothing) balance
      initialBalance = map (\(k,_) ->  (rValue k, rValue (100 :: Amount))) keysWhichDoesNotExistYet
      iBalance = map (\(key, val) -> Rocks.Put key val) initialBalance
      fun2 = (\db -> Rocks.write db def{Rocks.sync = True} iBalance)
  withResource db fun2
  --
  let fun3 = \(k,v) -> case v of Nothing -> (k, 100 :: Amount)
                                 Just balance -> case (urValue balance :: Either String Amount) of
                                   Left _  -> (k, 0)
                                   Right b -> (k, b)

      newBalance = map fun3 balance
  aBalanceTable <- H.fromList newBalance
  return aBalanceTable


runLedger :: DBPoolDescriptor -> InChan InfoMsg -> Microblock -> IO ()
runLedger db aInfoChan m = do
    let txs = _transactions m
    ht      <- getBalanceOfKeys (poolLedger db) txs
    mapM_ (updateBalanceTable ht) txs
    writeLedgerDB (poolLedger db) aInfoChan ht


getPubKeys :: Transaction -> [PublicKey]
getPubKeys (Transaction fromKey toKey _ _ _ _ _) = [fromKey, toKey]

-- type HashOfMicroblock = BC.ByteString
checkMacroblock :: DBPoolDescriptor -> InChan InfoMsg -> BC.ByteString -> BC.ByteString -> IO (Bool, Bool, Bool, Bool, Macroblock)
checkMacroblock db aInfoChan keyBlockHash blockHash = do
    let quantityMicroblocksInMacroblock = 2
        fun = (\db -> Rocks.get db Rocks.defaultReadOptions keyBlockHash)
    v  <- withResource (poolMacroblock db) fun

    case v of
      Nothing -> do -- If Macroblock is not already in the table, than insert it into the table
                    let aMacroBlock = dummyMacroblock {_mblocks = [blockHash]} :: Macroblock
                    return (True, False, True, True, aMacroBlock)
      Just a -> -- If Macroblock already in the table
        case urValue a :: Either String Macroblock of
          Left _  -> error "Can not decode Macroblock"
          Right bdMacroblock -> do
                   let hashes = _mblocks ( bdMacroblock :: Macroblock)
                   writeLog aInfoChan [BDTag] Info ("length hashes" ++ show(length hashes) ++ " " ++ show hashes)
                   if length hashes >= quantityMicroblocksInMacroblock
                   then if (length hashes == quantityMicroblocksInMacroblock)
                        then return (True, True, True, False, bdMacroblock)
                        else return (False, True, True, False, bdMacroblock)
                   else do
                     -- Check is Microblock already in Macroblock
                     let microblockIsAlreadyInMacroblock = blockHash `elem` hashes
                     writeLog aInfoChan [BDTag] Info ("Microblock is already in macroblock: " ++ show microblockIsAlreadyInMacroblock)
                     if microblockIsAlreadyInMacroblock
                       then return (True, False, False, True, bdMacroblock)  -- Microblock already in Macroblock - Nothing
                       else do -- add this Microblock to the value of Macroblock
                               let aMacroBlock = bdMacroblock {  _mblocks = hashes ++ [blockHash] } :: Macroblock
                               writeMacroblockToDB db aInfoChan keyBlockHash aMacroBlock
                             -- Check quantity of microblocks, can we close Macroblock?
                               if (length hashes >= (quantityMicroblocksInMacroblock - 1))
                                       then return (True, True, True, True, bdMacroblock)
                                       else return (True, False, True, True, bdMacroblock)


addMicroblockToDB :: DBPoolDescriptor -> Microblock -> InChan InfoMsg -> IO ()
addMicroblockToDB db m aInfoChan =  do
-- FIX: verify signature
    let txs = _transactions m
    let microblockHash = rHash m
    writeLog aInfoChan [BDTag] Info ("New Microblock came" ++ show(microblockHash))
-- FIX: Write to db atomically
    (goOn, macroblockClosed, microblockNew, macroblockNew, macroblock ) <- checkMacroblock db aInfoChan (_keyBlock (m :: Microblock)) microblockHash
    writeLog aInfoChan [BDTag] Info ("Macroblock - already closed " ++ show (not goOn))
    when goOn $ do
                writeLog aInfoChan [BDTag] Info ("Macroblock - New is " ++ show macroblockNew)
                writeLog aInfoChan [BDTag] Info ("Microblock - New is " ++ show microblockNew)
                writeLog aInfoChan [BDTag] Info ("Macroblock closed is " ++ show macroblockClosed)
                when (macroblockNew && microblockNew) $ do
                  writeMicroblockDB (poolMicroblock db) aInfoChan (transform m)
                  writeTransactionDB (poolTransaction db) aInfoChan txs microblockHash
                  -- let aMacroblock = fromJust macroblock

                  if macroblockClosed then do
                    -- get all microblocks (without the last added) for macroblock
                    let microblockHashes = init $ _mblocks (macroblock :: Macroblock)
                    mbBD <- mapM (\h -> getMicroBlockByHashDB db (Hash h))  microblockHashes
                    let realMb =  map fromJust (filter (isJust) mbBD)
                    mbWithTx <- mapM (transformation db) realMb
                    let sortedMb = sortBy (comparing _sign) ((mbWithTx ++ [m]) :: [Microblock])
                    writeLog aInfoChan [BDTag] Info ("Start calculate Ledger "  ++ show (length (sortedMb :: [Microblock])))
                    mapM_ (runLedger db aInfoChan) (sortedMb :: [Microblock])
                    writeMacroblockToDB db aInfoChan (_keyBlock (m :: Microblock)) (macroblock {_reward = 10})
                  else do
                    writeMacroblockToDB db aInfoChan (_keyBlock (m :: Microblock)) macroblock


writeMacroblockToDB :: DBPoolDescriptor -> InChan InfoMsg -> BC.ByteString -> Macroblock -> IO ()
writeMacroblockToDB desc aInfoChan keyBlock aMacroblock = do
  let key = keyBlock
      val = rValue aMacroblock
      fun = (\db -> Rocks.write db def{Rocks.sync = True} [ Rocks.Put key val ])
  withResource (poolMacroblock desc) fun
  writeLog aInfoChan [BDTag] Info ("Write Macroblock " ++ show key ++ "to DB")


writeMicroblockDB :: Pool Rocks.DB -> InChan InfoMsg -> MicroblockBD -> IO ()
writeMicroblockDB db aInfoChan m = do
  let key = rHash m
      val  = rValue m
      fun = (\db -> Rocks.write db def{Rocks.sync = True} [ Rocks.Put key val ])
  withResource db fun
  writeLog aInfoChan [BDTag] Info ("typeOf Microblock "  ++ (show (typeOf m)))
  writeLog aInfoChan [BDTag] Info ("Write Microblock "  ++ show key ++ "to Microblock table")


writeTransactionDB :: Pool Rocks.DB -> InChan InfoMsg -> [Transaction] -> BC.ByteString -> IO ()
writeTransactionDB dbTransaction aInfoChan txs hashOfMicroblock = do
  let txInfo = \tx1 num -> TransactionInfo tx1 hashOfMicroblock num
      txKeyValue = map (\(t,n) -> (rHash t, rValue (txInfo t n)) ) (zip txs [1..])
      fun = (\db -> Rocks.write db def{Rocks.sync = True} (map (\(k,v) -> Rocks.Put k v) txKeyValue))
  withResource dbTransaction fun
  writeLog aInfoChan [BDTag] Info ("Write Transactions to Transaction table")


writeLedgerDB ::  Pool Rocks.DB -> InChan InfoMsg -> BalanceTable -> IO ()
writeLedgerDB dbLedger aInfoChan bt = do
  ledgerKV <- H.toList bt
  let ledgerKeyValue = map (\(k,v)-> (rValue k, rValue v)) ledgerKV
      fun = (\db -> Rocks.write db def{Rocks.sync = True} (map (\(k,v) -> Rocks.Put k v) ledgerKeyValue))
  withResource dbLedger fun
  writeLog aInfoChan [BDTag] Info ("Write Ledger "  ++ show bt)



addMacroblockToDB :: DBPoolDescriptor -> Value -> InChan InfoMsg -> IO ()
addMacroblockToDB db aValue aInfoChan = do
  -- writeLog aInfoChan [BDTag] Info ("A.Value is " ++ show aValue)
  let (Object v) = aValue
      keyBlock = case parseMaybe extractKeyBlock v of
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
        fMacroblock = aMacroblock { _prevKBlock = prevHash, _difficulty = 20, _solver = aSolver, _timeK = atimeK, _numberK = aNumberK, _nonce = aNonce }

    writeMacroblockToDB db aInfoChan keyBlockHash fMacroblock


dummyMacroblock :: Macroblock
dummyMacroblock = Macroblock { _prevKBlock = "", _difficulty = 0, _height = 0, _solver = aSolver, _reward = 0, _mblocks = [], _timeK = 0, _numberK = 0, _nonce = 0}
  where aSolver = read "1" :: PublicKey


transformation :: DBPoolDescriptor -> MicroblockBD -> IO Microblock
transformation db m = do
  tx <- getTxsMicroblock db m
  return Microblock {
  _keyBlock      = _keyBlock (m :: MicroblockBD),
  _sign          = _signBD (m :: MicroblockBD),
  _teamKeys      = _teamKeys (m :: MicroblockBD),
  _transactions  = tx,
  _numOfBlock    = _numOfBlock (m :: MicroblockBD)
  }


transform :: Microblock -> MicroblockBD
transform m = MicroblockBD {
  _keyBlock = _keyBlock ( m :: Microblock) ,
  _signBD = _sign ( m :: Microblock),
  _teamKeys = _teamKeys ( m :: Microblock),
  _transactionsBD = map rHash (_transactions m),
  _numOfBlock = _numOfBlock ( m :: Microblock) }
