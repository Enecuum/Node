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
    addMacroblockToDB,
    runLedger,
    writeMacroblockToDB,
    writeTransactionDB,
    writeMicroblockDB,
    addMicroblockHashesToMacroBlock,
    calculateLedger,
    updateMacroblockByKeyBlock,
    tKBIPoW2KBI,
    tKeyBlockToPoWType
    ) where

import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Exception                     (throw)
import           Control.Monad                         (liftM, when)
import           Data.Aeson                            hiding (Error)
import           Data.Aeson.Types                      (parseMaybe)
import qualified Data.ByteString                       as B
import qualified Data.ByteString.Base64                as Base64
import qualified Data.ByteString.Char8                 as BC
import           Data.Default                          (def)
import           Data.Hashable
import qualified Data.HashTable.IO                     as H
import           Data.List                             (sort, sortBy)
import           Data.Maybe
import           Data.Ord                              (comparing)
import           Data.Pool
import qualified Data.Serialize                        as S (decode, encode)
import qualified Data.Set                              as Set
import           Data.Typeable
import qualified "rocksdb-haskell" Database.RocksDB    as Rocks
import           Node.Data.GlobalLoging
import           Service.Chan
import           Service.InfoMsg                       (InfoMsg (..),
                                                        LogingTag (..),
                                                        MsgType (..))
import           Service.Sync.SyncJson
import           Service.Transaction.Independent
import           Service.Transaction.Sprout
import           Service.Transaction.SproutCommon
import           Service.Transaction.Storage
import           Service.Types
import           Service.Types.PublicPrivateKeyPair
import           Service.Types.SerializeInstances      (roll, unroll)

instance Hashable PublicKey
type BalanceTable = H.BasicHashTable PublicKey Amount
type IsStorno = Bool
type IsMicroblockNew = Bool
type IsMacroblockNew = Bool



cReward :: Integer
cReward = 10


initialAmount :: Amount
initialAmount = 100

getBalanceForKey :: DBPoolDescriptor -> PublicKey -> IO (Maybe Amount)
getBalanceForKey db key = do
    val  <- funR (poolLedger db) (S.encode key)
    case val of Nothing -> return Nothing --putStrLn "There is no such key"
                Just v  -> case (S.decode v :: Either String Amount ) of
                    Left e  -> throw (DecodeException (show e))
                    Right b -> return $ Just b


updateBalanceTable :: DBPoolDescriptor -> BalanceTable -> IsStorno -> Transaction -> IO ()
updateBalanceTable db ht isStorno t@(Transaction fromKey toKey am _ _ _ _) = do
  v1 <- H.lookup ht $ fromKey
  v2 <- H.lookup ht $ toKey
  let tKey = rHashT t
  txI <- getTransactionByHashDB db (Hash tKey)
  case (v1,v2) of
    (Nothing, _)       -> do return ()
    (_, Nothing)       -> do return ()
    (Just balanceFrom, Just balanceTo) ->
      if (isStorno == False)
      then when ((balanceFrom - am) > 0) $ do
        H.insert ht fromKey (balanceFrom - am)
        H.insert ht toKey (balanceTo + am)
        updateTxStatus tKey txI True
      else do --it is storno transaction
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
      fun k = (\local_db -> Rocks.get local_db Rocks.defaultReadOptions (S.encode k))
      getBalanceByKey k = withResource db (fun k)
      toTuple k b = (,) k b
  balance  <- mapM (\k -> liftM (toTuple k ) (getBalanceByKey k)) hashKeys
  --   balance (key Maybe(S.encode Amount))

  let initialMoney = if isStorno then 0 else initialAmount

  -- initialize keys which does'not exist yet with initial balance
  when (not isStorno) $ do
    let keysWhichDoesNotExistYet = filter (\(_,v) -> v == Nothing) balance
        initialBalance = map (\(k,_) ->  (S.encode k, S.encode initialMoney)) keysWhichDoesNotExistYet
        iBalance = map (\(key, val) -> Rocks.Put key val) initialBalance
        fun2 = (\bd -> Rocks.write bd def{Rocks.sync = True} iBalance)
    withResource db fun2

  let fun3 = \(k,v) -> case v of Nothing -> (k, initialMoney )
                                 Just aBalance -> case (S.decode aBalance :: Either String Amount) of
                                   Left _  -> (k, 0)
                                   Right b -> (k, b)
  let newBalance = map fun3 balance
  aBalanceTable <- H.fromList newBalance
  return aBalanceTable


runLedger :: DBPoolDescriptor -> InChan InfoMsg -> IsStorno -> Microblock -> IO ()
runLedger db aInfoChan isStorno m = do
    let txs = _transactions m
    ht      <- getBalanceOfKeys (poolLedger db) isStorno txs
    mapM_ (updateBalanceTable db ht isStorno) txs
    writeLedgerDB (poolLedger db) aInfoChan ht


getPubKeys :: Transaction -> [PublicKey]
getPubKeys (Transaction fromKey toKey _ _ _ _ _) = [fromKey, toKey]


checkMacroblock :: DBPoolDescriptor -> InChan InfoMsg -> Microblock -> HashOfMicroblock -> IO (IsMicroblockNew, IsMacroblockNew, MacroblockBD)
checkMacroblock db aInfoChan microblock blockHash = do
    let keyBlockHash = (_keyBlock (microblock :: Microblock))
    v  <- funR (poolMacroblock db) keyBlockHash
    case v of
      Nothing -> do -- If MacroblockBD is not already in the table, than insert it into the table
                    let aMacroBlock = dummyMacroblock {
                          _mblocks = [blockHash],
                          _teamKeys = _teamKeys (microblock :: Microblock)
                          } :: MacroblockBD
                    return (True, True, aMacroBlock)
      Just a -> -- If MacroblockBD is already in the table
        case S.decode a :: Either String MacroblockBD of
          Left e  -> throw (DecodeException (show e))
          Right bdMacroblock -> do
                   let hashes = _mblocks ( bdMacroblock :: MacroblockBD)
                   writeLog aInfoChan [BDTag] Info ("length hashes" ++ show(length hashes) ++ " " ++ show hashes)
                   if (not $ checkMacroblockIsClosed bdMacroblock)
                   then do
                     -- Check is Microblock already in MacroblockBD
                        let microblockIsAlreadyInMacroblock = blockHash `elem` hashes
                        writeLog aInfoChan [BDTag] Info ("Microblock is already in macroblock: " ++ show microblockIsAlreadyInMacroblock)
                        if microblockIsAlreadyInMacroblock
                        then return (False, True, bdMacroblock)  -- Microblock already in MacroblockBD - Nothing
                        else do -- add this Microblock to the value of MacroblockBD
                               let aMacroBlock = bdMacroblock {  _mblocks = hashes ++ [blockHash] } :: MacroblockBD
                               writeMacroblockToDB db aInfoChan keyBlockHash aMacroBlock
                               return (True, True, bdMacroblock)
                    else return (True, True, bdMacroblock)


addMicroblockToDB :: DBPoolDescriptor -> Microblock -> InChan InfoMsg -> IO ()
addMicroblockToDB db m i =  do
-- FIX: verify signature
    let microblockHash = rHash $ tMicroblock2MicroblockBD  m
    writeLog i [BDTag] Info ("New Microblock came" ++ show(microblockHash))
-- FIX: Write to db atomically
    (isMicroblockNew, isMacroblockNew, macroblock ) <- checkMacroblock db i m microblockHash
    let isMacroblockClosed = checkMacroblockIsClosed macroblock
        goOn = macroblockIsOk macroblock
          where macroblockIsOk (MacroblockBD {..}) = length _mblocks <= length _teamKeys
    writeLog i [BDTag] Info ("MacroblockBD - already closed is " ++ show (not goOn))
    when goOn $ do
        writeLog i [BDTag] Info ("MacroblockBD - New is " ++ show isMacroblockNew)
        writeLog i [BDTag] Info ("Microblock - New is " ++ show isMicroblockNew)
        writeLog i [BDTag] Info ("MacroblockBD closed is " ++ show isMacroblockClosed)
        when (isMacroblockNew && isMicroblockNew) $ do
          writeMicroblockDB db i (tMicroblock2MicroblockBD m)
          writeTransactionDB db i (_transactions m) microblockHash
          writeMacroblockToDB db i (_keyBlock (m :: Microblock)) macroblock

          when isMacroblockClosed $ calculateLedger db i False (_keyBlock (m :: Microblock)) macroblock


calculateLedger :: DBPoolDescriptor -> InChan InfoMsg -> IsStorno -> HashOfKeyBlock -> MacroblockBD -> IO ()
calculateLedger db i isStorno hashKeyBlock macroblock = do
  -- get all microblocks for macroblock
  let microblockHashes = _mblocks (macroblock :: MacroblockBD)
  mbBD <- mapM (\h -> getMicroBlockByHashDB db (Hash h))  microblockHashes
  let realMb =  map fromJust (filter (isJust) mbBD)
  mbWithTx <- mapM (tMicroblockBD2Microblock db) realMb
  let sortedMb = sortBy (comparing _sign) (mbWithTx :: [Microblock])
      sortedM = if (isStorno == False) then sortedMb else reverse sortedMb
  writeLog i [BDTag] Info ("Start calculate Ledger, isStorno "  ++ show isStorno)
  mapM_ (runLedger db i isStorno) (sortedM :: [Microblock])
  case isStorno of False -> writeMacroblockToDB db i hashKeyBlock (macroblock {_reward = cReward})
                   True -> do
                     let aReward = (_reward (macroblock :: MacroblockBD)) - cReward
                     writeMacroblockToDB db i hashKeyBlock (macroblock {_reward = aReward})



writeMacroblockToDB :: DBPoolDescriptor -> InChan InfoMsg -> BC.ByteString -> MacroblockBD -> IO ()
writeMacroblockToDB desc a hashOfKeyBlock aMacroblock = do
  hashPreviousLastKeyBlock <- funR (poolMacroblock desc) lastClosedKeyBlock
  let cMacroblock = if (checkMacroblockIsClosed aMacroblock == True)
        then (aMacroblock { _prevKBlock = hashPreviousLastKeyBlock }) :: MacroblockBD
        else aMacroblock
  let cKey = hashOfKeyBlock
      cVal = (S.encode cMacroblock)
  funW (poolMacroblock desc) [(cKey,cVal)]
  writeLog a [BDTag] Info ("Write Macroblock " ++ show cKey ++ " " ++ show cMacroblock ++ "to DB")

  -- For closed Macroblock
  when (checkMacroblockIsClosed aMacroblock) $ do
    -- fill _nextKBlock for previous closed Macroblock
    bdKV <- case hashPreviousLastKeyBlock of
      Nothing -> return []
      Just j  -> do
        previousLastKeyBlock <- funR (poolMacroblock desc) j
        case previousLastKeyBlock of
          Nothing -> return []
          Just k -> case S.decode k :: Either String MacroblockBD of
                      Left e -> do
                        writeLog a [BDTag] Error ("Can not decode Macroblock" ++ show e)
                        return []
                      Right r -> do
                        let pKey = j
                            pVal = S.encode $ (r { _nextKBlock = Just hashOfKeyBlock } :: MacroblockBD)
                        return [(pKey, pVal)]

    -- fill new last closed Macroblock
    let keyValue = (lastClosedKeyBlock, cKey) : bdKV
    funW (poolMacroblock desc) keyValue
    writeLog a [BDTag] Info ("Write Last Closed Macroblock " ++ show lastClosedKeyBlock ++ "to DB")


writeMicroblockDB :: DBPoolDescriptor -> InChan InfoMsg -> MicroblockBD -> IO ()
writeMicroblockDB descr aInfoChan m = do
  let db = poolMicroblock descr
      key = rHash m
      val  = S.encode m
  funW db [(key,val)]
  writeLog aInfoChan [BDTag] Info ("Write Microblock "  ++ show key ++ "to Microblock table")


writeTransactionDB :: DBPoolDescriptor -> InChan InfoMsg -> [Transaction] -> BC.ByteString -> IO ()
writeTransactionDB descr aInfoChan txs hashOfMicroblock = do
  let db = poolTransaction descr
      txInfo = \tx1 num -> TransactionInfo tx1 hashOfMicroblock num False
      txKeyValue = map (\(t,n) -> (rHashT t , S.encode (txInfo t n)) ) (zip txs [1..])
  funW db txKeyValue
  writeLog aInfoChan [BDTag] Info ("Write Transactions to Transaction table")
  writeLog aInfoChan [BDTag] Info $ "Transactions: " ++ show (map (\t -> rHash t { _timestamp = Nothing }) txs)


writeLedgerDB ::  Pool Rocks.DB -> InChan InfoMsg -> BalanceTable -> IO ()
writeLedgerDB dbLedger aInfoChan bt = do
  ledgerKV <- H.toList bt
  let ledgerKeyValue = map (\(k,v)-> (S.encode k, S.encode v)) ledgerKV
  funW dbLedger ledgerKeyValue
  writeLog aInfoChan [BDTag] Info ("Write Ledger "  ++ show bt)


addMacroblockToDB :: DBPoolDescriptor -> Value -> InChan InfoMsg -> (InChan SyncEvent, b) -> IO ()
addMacroblockToDB db (Object aValue) aInfoChan  aSyncChan = do
  let keyBlock = case parseMaybe (.: "verb") aValue of
        Nothing     -> throw (DecodeException "There is no verb in PoW Key Block")
        Just kBlock -> kBlock :: BC.ByteString --Map T.Text Value
  if keyBlock /= "kblock"
    then return ()
    else do
    let body = case parseMaybe (.: "body") aValue of
          Nothing     -> throw (DecodeException "Can not parse body of PoW Key Block ")
          Just kBlock -> kBlock :: BC.ByteString --BSI.ByteString --KeyBlockInfo --Map T.Text Value

    let decodedKey = Base64.decode body -- :: Either String
    print decodedKey
    case decodedKey of
      Left e -> throw (DecodeException (show e))
      Right r -> do
        case Data.Aeson.eitherDecodeStrict $ BC.init $ BC.tail r of
          Left a -> throw (DecodeException $ "There is no PoW Key Block. The error: " ++ a)
          Right (keyBlockInfo ) -> do
            putStrLn ("type of keyBlockInfoObject is: " ++ (show (typeOf keyBlockInfo)))
            print keyBlockInfo
            let aKeyBlock = tKBIPoW2KBI keyBlockInfo
            let aKeyBlockHash = getKeyBlockHash keyBlockInfo
            putStrLn $ "keyBlockHash" ++ show aKeyBlockHash
            writeLog aInfoChan [BDTag] Info (show keyBlockInfo)

            let receivedKeyNumber = _number (keyBlockInfo :: KeyBlockInfoPoW)
                startSync = writeInChan (fst aSyncChan) RestartSync
            currentNumberInDB <- getKeyBlockNumber (Common db aInfoChan)
            let updateKeyBlockDB = updateMacroblockByKeyBlock db aInfoChan aKeyBlockHash aKeyBlock Main
            case currentNumberInDB of
              Nothing -> updateKeyBlockDB
              Just j  -> do
                hashOfDBKeyBlock <- getM (Common db aInfoChan) j
                case hashOfDBKeyBlock of
                  Nothing ->  writeLog aInfoChan [BDTag] Error ("There is no key block with number " ++ (show j))
                  Just h -> if (h == _prev_hash (aKeyBlock :: KeyBlockInfo))
                    then updateKeyBlockDB
                    else when (j < receivedKeyNumber) $ do startSync
addMacroblockToDB _ _ _ _ = error "Can not PoW Key Block"

tKBIPoW2KBI :: KeyBlockInfoPoW -> KeyBlockInfo
tKBIPoW2KBI (KeyBlockInfoPoW {..}) = KeyBlockInfo {
  _time,
  _prev_hash,
  _number,
  _nonce,
  _solver = pubKey,
  _type}
  where pubKey = publicKey256k1 ((roll $ B.unpack _solver) :: Integer)


tKeyBlockToPoWType :: KeyBlockInfo -> KeyBlockInfoPoW
tKeyBlockToPoWType (KeyBlockInfo {..}) = KeyBlockInfoPoW{
  _time,
  _prev_hash,
  _number,
  _nonce,
  _solver = pubKey,
  _type}
  where pubKey = B.pack $ unroll $ fromPublicKey256k1 _solver


updateMacroblockByKeyBlock :: DBPoolDescriptor -> InChan InfoMsg -> HashOfKeyBlock -> KeyBlockInfo -> BranchOfChain -> IO ()
updateMacroblockByKeyBlock db aInfoChan hashOfKeyBlock keyBlockInfo branch = do
    val  <- funR (poolMacroblock db) hashOfKeyBlock
    _ <- case val of
        Nothing -> return dummyMacroblock
        Just va  -> case S.decode va :: Either String MacroblockBD of
            Left e  -> throw (DecodeException (show e))
            Right r -> return r

    writeMacroblockToDB db aInfoChan hashOfKeyBlock $ tKeyBlockInfo2Macroblock keyBlockInfo
    setChain (Common db aInfoChan ) (_number (keyBlockInfo :: KeyBlockInfo)) hashOfKeyBlock branch

addMicroblockHashesToMacroBlock :: DBPoolDescriptor -> InChan InfoMsg -> HashOfKeyBlock -> [HashOfMicroblock] -> IO ()
addMicroblockHashesToMacroBlock db i hashOfKeyBlock hashesOfMicroblock = do
  val  <- funR (poolMacroblock db) hashOfKeyBlock
  case val of
    Nothing -> writeLog i [BDTag] Error ("There is no KeyBlock with hash " ++ show hashOfKeyBlock)
    Just k -> case S.decode k :: Either String MacroblockBD of
      Left e  -> throw (DecodeException (show e))
      Right (r )  -> do
        let currentHashes = Set.fromList $ _mblocks (r :: MacroblockBD)
            newHashes = Set.fromList $ hashesOfMicroblock
            allHashes = sort $ Set.elems $ Set.union currentHashes newHashes
        let macroblock = S.encode $ (r {_mblocks = allHashes} :: MacroblockBD)
        funW (poolMacroblock db) [(hashOfKeyBlock, macroblock)]
