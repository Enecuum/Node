{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PackageImports           #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# OPTIONS_GHC -fno-warn-orphans     #-}

module Service.Transaction.Balance
  ( getBalanceForKey,
    addMicroblockToDB,
    addKeyBlockToDB,
    runLedger,
    writeMacroblockToDB,
    writeTransactionDB,
    writeMicroblockDB,
    addMicroblockHashesToMacroBlock,
    calculateLedger,
    updateMacroblockByKeyBlock,
    tKBIPoW2KBI,
    tKeyBlockToPoWType,
    addKeyBlockToDB2
    ) where

import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Exception                     (throw)
import           Control.Monad                         (liftM, when)
import           Data.Aeson                            hiding (Error)
import           Data.Default                          (def)
import           Data.Hashable
import qualified Data.HashTable.IO                     as H
import           Data.List                             (sort, sortBy)
import           Data.Maybe
import           Data.Ord                              (comparing)
import           Data.Pool
import qualified Data.Serialize                        as S (decode, encode)
import qualified Data.Set                              as Set
import qualified "rocksdb-haskell" Database.RocksDB    as Rocks
import           Node.Data.GlobalLoging
import           Service.Chan
import           Service.InfoMsg                       (InfoMsg (..),
                                                        LogingTag (..),
                                                        MsgType (..))
import           Service.Sync.SyncJson
import           Service.Transaction.Decode
import           Service.Transaction.Sprout
import           Service.Transaction.Storage
import           Service.Transaction.Transformation
import           Service.Types
import           Service.Types.PublicPrivateKeyPair


instance Hashable PublicKey
type BalanceTable = H.BasicHashTable PublicKey Amount
type IsStorno = Bool
type IsMicroblockNew = Bool
type IsMacroblockNew = Bool



-- cReward :: Integer
-- cReward = 0


initialAmount :: Amount
initialAmount = 100


updateBalanceTable :: Common  -> BalanceTable -> IsStorno -> Transaction -> IO ()
updateBalanceTable (Common db i) ht isStorno t@(Transaction fromKey toKey am _ _ _ _) = do
  v1 <- H.lookup ht fromKey
  v2 <- H.lookup ht toKey
  let tKey = rHashT t
  txI <- getTransactionByHashDB db (Hash tKey)
  writeLog i [BDTag] Info $ "Transaction " ++ show txI ++ " isStorno " ++ show isStorno
  case (v1,v2) of
    (Nothing, _)       -> return ()
    (_, Nothing)       -> return ()
    (Just balanceFrom, Just balanceTo) ->
      if not isStorno
      then when ((balanceFrom - am) > 0) $ do
        -- writeLog i [BDTag] Info $ "Forward tx: fromKey " ++ show fromKey ++ " toKey " ++ show toKey ++ " - amount " ++ show am
        H.insert ht fromKey (balanceFrom - am)
        H.insert ht toKey (balanceTo + am)
        updateTxStatus tKey txI True
      else do --it is storno transaction
        -- writeLog i [BDTag] Info $ "Storno tx: fromKey " ++ show fromKey ++ " toKey " ++ show toKey ++ " + amount " ++ show am
        H.insert ht fromKey (balanceFrom + am)
        H.insert ht toKey (balanceTo - am)
        updateTxStatus tKey txI False
  where
     updateTxStatus tKey txI stat = case txI of
         Nothing   -> throw DBTransactionException
         Just info -> funW (poolTransaction db) [(tKey,  S.encode (info { _accepted = stat}))]


getBalanceOfKeys :: Pool Rocks.DB -> IsStorno -> [Transaction] -> IO BalanceTable
getBalanceOfKeys db isStorno tx = do
  let hashKeys = concatMap getPubKeys tx
      fun k db = Rocks.get db Rocks.defaultReadOptions (S.encode k)
      getBalanceByKey k = withResource db (fun k)
      toTuple k b = (,) k b
  balance  <- mapM (\k -> liftM (toTuple k ) (getBalanceByKey k)) hashKeys
  --   balance (key Maybe(S.encode Amount))

  let initialMoney = if isStorno then 0 else initialAmount

  -- initialize keys which does'not exist yet with initial balance
  when (not isStorno) $ do
    let keysWhichDoesNotExistYet = filter (\(_,v) -> isNothing v ) balance
        initialBalance = map (\(k,_) ->  (S.encode k, S.encode initialMoney)) keysWhichDoesNotExistYet
        iBalance = map (uncurry Rocks.Put) initialBalance
        fun2 bd = Rocks.write bd def{Rocks.sync = True} iBalance
    withResource db fun2

  let fun3 (k,v) = case v of Nothing -> (k, initialMoney )
                             Just aBalance -> case (S.decode aBalance :: Either String Amount) of
                               Left _  -> (k, 0)
                               Right b -> (k, b)
  let newBalance = map fun3 balance
  aBalanceTable <- H.fromList newBalance
  return aBalanceTable


runLedger :: Common -> IsStorno -> Microblock -> IO ()
runLedger c@(Common db i) isStorno m = do
    let txs = _transactions m
    writeLog i [BDTag] Info $ "runLedger, isStorno: " ++ show isStorno ++ " for transactions: " ++ show txs
    ht      <- getBalanceOfKeys (poolLedger db) isStorno txs
    mapM_ (updateBalanceTable c ht isStorno) txs
    writeLedgerDB (poolLedger db) i ht


getPubKeys :: Transaction -> [PublicKey]
getPubKeys (Transaction fromKey toKey _ _ _ _ _) = [fromKey, toKey]


checkMacroblock :: Common -> Microblock -> HashOfMicroblock -> IO (IsMicroblockNew, IsMacroblockNew, MacroblockBD)
checkMacroblock (Common db i) microblock microblockHash = do
    let keyBlockHash = (_keyBlock (microblock :: Microblock))
    mb <- getKeyBlockByHash db i (Hash keyBlockHash)
    let mbUpdated = case mb of
          Nothing -> dummyMacroblock { _teamKeys = _teamKeys (microblock :: Microblock)} :: MacroblockBD
          Just j -> j {_teamKeys = _teamKeys (microblock :: Microblock)} :: MacroblockBD

    let hashes = _mblocks ( mbUpdated :: MacroblockBD)
        mes = ("length hashes" ++ show(length hashes) ++ " " ++ show hashes)
    writeLog i [BDTag] Info mes
    --Check is Microblock already in MacroblockBD
    if microblockHash `elem` hashes -- microblockIsAlreadyInMacroblock
      then do
      writeLog i [BDTag] Info $ "Microblock is already in macroblock: " ++ show True
      return (False, True, mbUpdated)  -- Microblock already in MacroblockBD - do Nothing
      else return (True, True, mbUpdated)


-- main function for Microblock
addMicroblockToDB :: Common -> Microblock -> IO ()
addMicroblockToDB c@(Common _ i) m =  do
-- FIX: verify signature
    let microblockHash = rHash $ tMicroblock2MicroblockBD  m
    writeLog i [BDTag] Info $ "New Microblock came" ++ show microblockHash
-- FIX: Write to db atomically
    (isMicroblockNew, isMacroblockNew, macroblock ) <- checkMacroblock c m microblockHash
    aIsMacroblockClosed <- isMacroblockClosed macroblock i
    -- let goOn = not aIsMacroblockClosed
    let goOn = macroblockIsOk macroblock
          where macroblockIsOk MacroblockBD {..} = length _mblocks <= length _teamKeys
    writeLog i [BDTag] Info ("MacroblockBD :- length _mblocks <= length _teamKeyss " ++ show (not goOn))
    writeLog i [BDTag] Info $ "Are we going to process microblock? - " ++ show goOn
    when goOn $ do
        writeLog i [BDTag] Info ("MacroblockBD - New is " ++ show isMacroblockNew)
        writeLog i [BDTag] Info ("Microblock - New is " ++ show isMicroblockNew)
        writeLog i [BDTag] Info ("MacroblockBD closed is " ++ show aIsMacroblockClosed)
        when (isMacroblockNew && isMicroblockNew) $ do
          writeMacroblockToDB c (_keyBlock (m :: Microblock)) macroblock
          writeMicroblockDB c (tMicroblock2MicroblockBD m)
          writeTransactionDB c (_transactions m) microblockHash
          -- writeMacroblockToDB db i (_keyBlock (m :: Microblock)) macroblock
          when aIsMacroblockClosed $ calculateLedger c False (_keyBlock (m :: Microblock)) macroblock


calculateLedger :: Common -> IsStorno -> HashOfKeyBlock -> MacroblockBD -> IO ()
calculateLedger c@(Common db i) isStorno _ macroblock = do
  -- get all microblocks for macroblock
  let microblockHashes = _mblocks (macroblock :: MacroblockBD)
  mbBD <- mapM (getMicroBlockByHashDB db . Hash)  microblockHashes
  mbWithTx <- mapM (tMicroblockBD2Microblock db i) mbBD
  let sortedMb = sortBy (comparing _sign) (mbWithTx :: [Microblock])
      sortedM = if not isStorno then sortedMb else reverse sortedMb
  writeLog i [BDTag] Info $ "calculateLedger: microblockHashes " ++ show sortedM
  writeLog i [BDTag] Info ("calculateLedger: Start calculate Ledger, isStorno "  ++ show isStorno)
  mapM_ (runLedger c isStorno) (sortedM :: [Microblock])
  -- case isStorno of False -> writeMacroblockToDB db i hashKeyBlock (macroblock {_reward = cReward})
  --                  True -> do
  --                    let aReward = (_reward (macroblock :: MacroblockBD)) - cReward
  --                    writeMacroblockToDB db i hashKeyBlock (macroblock {_reward = aReward})


writeMicroblockDB :: Common -> MicroblockBD -> IO ()
writeMicroblockDB c@(Common descr i) m = do
  let db = poolMicroblock descr
      hashOfMicroblock = rHash m
      val  = S.encode m
  funW db [(hashOfMicroblock,val)]
  let mes = foldr1 (++) ["Write Microblock ", show hashOfMicroblock, "to Microblock table"]
  writeLog i [BDTag] Info mes
  let hashKeyBlock = _keyBlock (m :: MicroblockBD)
  writeLog i [BDTag] Info $ "Going to add microblock hashOfMicroblock " ++ show hashOfMicroblock ++ "to key block " ++ show hashKeyBlock
  addMicroblockHashesToMacroBlock c hashKeyBlock [hashOfMicroblock]


writeTransactionDB :: Common -> [Transaction] -> HashOfMicroblock -> IO ()
writeTransactionDB (Common descr aInfoChan) txs hashOfMicroblock = do
  let db = poolTransaction descr
      txInfo tx1 num = TransactionInfo tx1 hashOfMicroblock num False
      txKeyValue = map (\(t,n) -> (rHashT t , S.encode (txInfo t n)) ) (zip txs [1..])
  funW db txKeyValue
  writeLog aInfoChan [BDTag] Info "Write Transactions to Transaction table"
  writeLog aInfoChan [BDTag] Info $ "Transactions: " ++ show (map (\t -> rHash t { _timestamp = Nothing }) txs)


writeLedgerDB ::  Pool Rocks.DB -> InChan InfoMsg -> BalanceTable -> IO ()
writeLedgerDB dbLedger aInfoChan bt = do
  ledgerKV <- H.toList bt
  let ledgerKeyValue = map (\(k,v)-> (S.encode k, S.encode v)) ledgerKV
  funW dbLedger ledgerKeyValue
  writeLog aInfoChan [BDTag] Info ("Write Ledger "  ++ show bt)



addKeyBlockToDB2 :: Common  -> KeyBlockInfoPoW -> (InChan SyncEvent, b) -> IO ()
addKeyBlockToDB2 c@(Common db i) keyBlockInfo aSyncChan = do
    let aKeyBlock = tKBIPoW2KBI keyBlockInfo
        aKeyBlockHash = getKeyBlockHash keyBlockInfo

    writeLog i [BDTag] Info $ "keyBlockHash: " ++ show aKeyBlockHash
    writeLog i [BDTag] Info $ "keyBlockInfo: " ++ show aKeyBlock

    let receivedKeyNumber = _number (keyBlockInfo :: KeyBlockInfoPoW)
        startSync = writeInChan (fst aSyncChan) RestartSync
    currentNumberInDB <- getKeyBlockNumber (Common db i)
    writeLog i [BDTag] Info $ "Current KeyBlock Number In DB is " ++ show currentNumberInDB
    case currentNumberInDB of
      Nothing -> writeLog i [BDTag] Error "There are no genesis key block number!"
      Just j  -> do
        when (j < receivedKeyNumber) $ do
          hashOfDBKeyBlock <- getM (Common db i) j
          writeLog i [BDTag] Info $ "Current hash of KeyBlock in DB is " ++ show hashOfDBKeyBlock
          let prev_hash = _prev_hash (aKeyBlock :: KeyBlockInfo)
          case hashOfDBKeyBlock of
            Nothing ->  writeLog i [BDTag] Error $ "There is no key block with number " ++ show j
            Just h -> if h == prev_hash
              then updateMacroblockByKeyBlock c aKeyBlockHash aKeyBlock Main
              else do
              let mes = "Hashes doesn't much: current hash: " ++ show h ++ "previous hash: " ++ show prev_hash
              writeLog i [BDTag] Info mes
              when (j < receivedKeyNumber) startSync


addKeyBlockToDB :: Common -> Value -> (InChan SyncEvent, b) -> IO ()
addKeyBlockToDB c@(Common _ i) o aSyncChan = do
  keyBlockInfo <- decodeKeyBlock i o
  addKeyBlockToDB2 c keyBlockInfo aSyncChan


addMicroblockHashesToMacroBlock :: Common -> HashOfKeyBlock -> [HashOfMicroblock] -> IO ()
addMicroblockHashesToMacroBlock (Common db i) hashOfKeyBlock hashesOfMicroblock = do
  val <- getKeyBlockByHash db i (Hash hashOfKeyBlock)
  macroblock <- S.encode <$> case val of
    Nothing -> do
      writeLog i [BDTag] Info ("There is no KeyBlock with hash " ++ show hashOfKeyBlock)
      return (dummyMacroblock {_mblocks = hashesOfMicroblock} :: MacroblockBD)
    Just j -> do
      let currentHashes = Set.fromList $ _mblocks (j :: MacroblockBD)
          newHashes = Set.fromList hashesOfMicroblock
          allHashes = sort $ Set.elems $ Set.union currentHashes newHashes
      return (j {_mblocks = allHashes} :: MacroblockBD)
  funW (poolMacroblock db) [(hashOfKeyBlock, macroblock)]
  let mes = foldr1 (++) ["Write hashes microblocks ", show hashesOfMicroblock, " to key block ", show hashOfKeyBlock]
  writeLog i [BDTag] Info mes
