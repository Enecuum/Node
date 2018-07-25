{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Service.Transaction.Storage where

import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Exception
import           Control.Monad                         (forM, replicateM, when)
import qualified Control.Monad.Catch                   as E
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State             (StateT, evalStateT, get,
                                                        put)
import           Control.Retry
import qualified Crypto.Hash.SHA256                    as SHA
import qualified Data.ByteString                       as B
import qualified Data.ByteString.Base64                as Base64
import qualified Data.ByteString.Internal              as BSI
import           Data.Default                          (def)
import           Data.Either
import           Data.Pool
import qualified Data.Serialize                        as S (Serialize, decode,
                                                             encode)
import           Data.Serialize.Put
import qualified "rocksdb-haskell" Database.RocksDB    as Rocks
import           Node.Data.GlobalLoging
-- import           Service.InfoMsg                       (LogingTag (..),
--                                                         MsgType (..))
import           Service.System.Directory
import           Service.Types
import           Service.Types.PublicPrivateKeyPair
-- import           Service.Types.SerializeInstances      (roll, unroll)
import           Service.InfoMsg                       (InfoMsg (..),
                                                        LogingTag (..),
                                                        MsgType (..))
import           Service.Transaction.Decode
import           Service.Types.SerializeInstances      (roll)
import           Service.Types.SerializeJSON           ()

--------------------------------------


-- FIX change def (5 times)
connectOrRecoveryConnect :: IO DBPoolDescriptor
connectOrRecoveryConnect = recovering def handler . const $ connectDB


connectDB :: IO DBPoolDescriptor
connectDB = do
  let fun dbFilePath = createPool (Rocks.open dbFilePath def{Rocks.createIfMissing=True}) Rocks.close 1 32 16
  poolTransaction <- fun =<< getTransactionFilePath
  poolMicroblock  <- fun =<< getMicroblockFilePath
  poolLedger      <- fun =<< getLedgerFilePath
  poolMacroblock  <- fun =<< getMacroblockFilePath
  poolSprout      <- fun =<< getSproutFilePath
  -- putStrLn "DBTransactionException"
  -- sleepMs 5000
  -- throw DBTransactionException
  return (DBPoolDescriptor poolTransaction poolMicroblock poolLedger poolMacroblock poolSprout)


data SuperException = DBTransactionException
                    | NotImplementedException -- test
                    | OtherException
                  deriving (Show)

instance Exception SuperException


--catch all exceptions and retry connections
handler :: [p -> E.Handler IO Bool]
handler =
    [ \_ -> E.Handler $ \(_ :: SomeException) -> do
        return True
    ]

-- End of the Connection section
--------------------------------------


--------------------------------------
-- begin of the Database structure  section

-- for rocksdb Transaction and Microblock
rHashT :: Transaction -> BSI.ByteString
rHashT t@(Transaction {}) = Base64.encode . SHA.hash . S.encode $ t { _timestamp = Nothing }

rHash :: S.Serialize a => a -> BSI.ByteString
rHash key = Base64.encode . SHA.hash . S.encode $ key

lastClosedKeyBlock :: DBKey
lastClosedKeyBlock = "2dJ6lb9JgyQRac0DAkoqmYmS6ats3tND0gKMLW6x2x8=" :: DBKey




checkMacroblockIsClosed :: MacroblockBD -> Bool
checkMacroblockIsClosed MacroblockBD {..} = length _mblocks /= 0 && length _teamKeys == length _mblocks

-- end of the Database structure  section
--------------------------------------


getTxs :: DBPoolDescriptor -> InChan InfoMsg -> MicroblockBD -> IO [TransactionInfo]
getTxs descr i mb = do
  let txHashes = _transactionsHashes mb
  forM txHashes $ getTx1 descr i
  where getTx1 :: DBPoolDescriptor -> InChan InfoMsg -> HashOfTransaction -> IO TransactionInfo
        getTx1 d _ h = do
          maybeTxUntyped  <- funR (poolTransaction d) h
          case maybeTxUntyped of
            Nothing -> throw $ NoSuchTransactionForHash ("hash: " ++ show h)
            Just t -> case S.decode t :: Either String TransactionInfo of
              Left  e -> throw (DecodeException (show e))
              Right r -> return r


getTxsMicroblock :: DBPoolDescriptor -> InChan InfoMsg -> MicroblockBD -> IO [Transaction]
getTxsMicroblock db i mb = do
  txDecoded <- getTxs db i mb
  let tx = map (\t -> _tx  (t :: TransactionInfo)) txDecoded
  return tx


getNFirstValuesT :: StateT Rocks.Iterator IO DBValue
getNFirstValuesT = do
  it <- get
  Just v <- Rocks.iterValue it
  Rocks.iterNext it
  put it
  return v


getNFirstValues :: (MonadTrans t, MonadResource (t IO)) => Rocks.DB -> Int -> t IO [DBValue]
getNFirstValues db n = do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterFirst it
  lift $ evalStateT (replicateM n getNFirstValuesT) it


getNLastValuesT :: StateT Rocks.Iterator IO DBValue
getNLastValuesT = do
  it <- get
  Just v <- Rocks.iterValue it
  Rocks.iterPrev it
  put it
  return v


getNLastValues :: Rocks.DB -> Int -> IO [DBValue]
getNLastValues db n = runResourceT $ do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterLast it
  lift $ evalStateT (replicateM n getNLastValuesT) it


getFirst :: (MonadResource (t IO), MonadTrans t) => Rocks.DB -> Int -> Int -> t IO [DBValue]
getFirst db offset count = drop offset <$> getNFirstValues db (offset + count )

getLast :: Rocks.DB -> Int -> Int -> IO [(DBKey, DBValue)]
-- getLast db  offset count = drop offset <$> getNLastValues db (offset + count )
getLast db  offset count = drop offset <$> getNLastValues2 db (offset + count )


getNLastValues2 :: Rocks.DB -> Int -> IO [(DBKey, DBValue)]
getNLastValues2 db n = runResourceT $ do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterLast it
  _ <- replicateM (n - 1) $ Rocks.iterPrev it
  Rocks.iterItems it


getChainInfoDB :: DBPoolDescriptor -> InChan InfoMsg -> IO ChainInfo
getChainInfoDB desc aInfoChan = do
  kv <- getLastKeyBlock desc aInfoChan
  tMacroblock2ChainInfo kv


lastKeyBlock :: DBKey
lastKeyBlock = "OvS8LmmcMa4mtEWbifO5ZFkqT6AYRizzQ6mEobMMhz4=" :: DBKey

getLastKeyBlock  :: DBPoolDescriptor -> InChan InfoMsg -> IO (Maybe (DBKey,MacroblockBD))
getLastKeyBlock desc aInfoChan = do
  key <- funR (poolMacroblock desc) lastKeyBlock
  case key of Nothing -> return Nothing
              Just k  -> do
                mb <- getKeyBlockByHash desc aInfoChan (Hash k)
                case mb of Nothing -> do
                                writeLog aInfoChan [BDTag] Error "No Key block "
                                return Nothing
                           Just r -> return $ Just (k,r)


getLastTransactions :: DBPoolDescriptor -> PublicKey -> Int -> Int -> IO [TransactionAPI]
getLastTransactions descr pubKey offset aCount = do
  let fun = \db -> getLast db offset aCount
  txs <- withResource (poolTransaction descr) fun
  let rawTxInfo = map (\(_,v) -> v) txs
  let txAPI = decodeTransactionsAndFilterByKey rawTxInfo pubKey
  return txAPI


getMicroBlockByHashDB :: DBPoolDescriptor -> Hash -> IO MicroblockBD
getMicroBlockByHashDB db mHash = do
  mbByte <- getByHash (poolMicroblock db) mHash
  case mbByte of Nothing -> throw (NoSuchMicroBlockForHash $ show mHash)
                 Just m -> case (S.decode m :: Either String MicroblockBD) of
                   Left e  -> throw (DecodeException (show e))
                   Right r -> return r


getTransactionsByMicroblockHash :: DBPoolDescriptor -> InChan InfoMsg -> Hash -> IO (Maybe [TransactionInfo])
getTransactionsByMicroblockHash db i aHash = do
  m@(MicroblockBD {..}) <- getMicroBlockByHashDB db aHash
  -- case mb of
  --   Nothing -> return Nothing
  --   Just m@(MicroblockBD {..}) -> do
  txInfo <- getTxs db i m
  return $ Just txInfo

getBlockByHashDB :: DBPoolDescriptor -> Hash -> InChan InfoMsg -> IO (Maybe MicroblockAPI)
getBlockByHashDB db hash i = do
  m <- getMicroBlockByHashDB db hash
  -- case mb of
  --   Nothing -> return Nothing
  --   Just m  -> do
  mAPI <- tMicroblockBD2MicroblockAPI db i m --aInfoChan
  return $ Just mAPI




getKeyBlockByHashDB :: DBPoolDescriptor -> Hash -> InChan InfoMsg -> IO (Maybe MacroblockAPI)
getKeyBlockByHashDB db kHash i = do
  hashOfKey <- getKeyBlockByHash db i kHash
  case hashOfKey of Nothing -> return Nothing
                    Just j  -> Just <$> (tMacroblock2MacroblockAPI db j)


getTransactionByHashDB :: DBPoolDescriptor -> Hash -> IO (Maybe TransactionInfo) --Transaction
getTransactionByHashDB db tHash = do
  tx <- getByHash (poolTransaction db) tHash
  case tx of Nothing -> return Nothing
             Just j -> case (S.decode j :: Either String  TransactionInfo) of
               Left e   -> throw (DecodeException (show e))
               Right rt -> return $ Just rt





decodeTransactionsAndFilterByKey :: [DBValue] -> PublicKey -> [TransactionAPI]
decodeTransactionsAndFilterByKey rawTx pubKey = txAPI
  where fun = \t -> case (S.decode t :: Either String TransactionInfo) of
                       Left e  -> throw (DecodeException (show e))
                       Right r -> _tx (r  :: TransactionInfo)

        tx = map fun rawTx
        txWithKey = filter (\t -> (_owner t == pubKey || _receiver t == pubKey)) tx
        txAPI = map (\t -> TransactionAPI { _tx = t, _txHash = rHashT t}) txWithKey


getAllTransactionsDB :: DBPoolDescriptor -> PublicKey -> IO [TransactionAPI]
getAllTransactionsDB descr pubKey = do
  txByte <- withResource (poolTransaction descr) getAllValues
  return $ decodeTransactionsAndFilterByKey txByte pubKey


getAllValues :: MonadUnliftIO m => Rocks.DB -> m [DBValue]
getAllValues db = runResourceT $ do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterFirst it
  Rocks.iterValues it


getAllItems :: MonadResource m => Rocks.DB -> m [(DBKey, DBValue)]
getAllItems db = do
  it    <- Rocks.iterOpen db Rocks.defaultReadOptions
  Rocks.iterFirst it
  Rocks.iterItems it


tMicroblockBD2Microblock :: DBPoolDescriptor -> InChan InfoMsg -> MicroblockBD -> IO Microblock
tMicroblockBD2Microblock db i m@(MicroblockBD {..}) = do
  tx <- getTxsMicroblock db i m
  aTeamkeys <- getTeamKeysForMicroblock db i _keyBlock
  return Microblock {
  _keyBlock,
  _sign          = _signBD,
  -- _teamKeys,
  _teamKeys = aTeamkeys,
  _publisher,
  _transactions  = tx
  -- _numOfBlock
  }

tMicroblock2MicroblockBD :: Microblock -> MicroblockBD
tMicroblock2MicroblockBD (Microblock {..}) = MicroblockBD {
  _keyBlock,
  _signBD = _sign,
  -- _teamKeys,
  _publisher,
  _transactionsHashes = map rHashT _transactions
  -- _numOfBlock = 0
  }


getTeamKeysForMicroblock :: DBPoolDescriptor -> InChan InfoMsg -> HashOfKeyBlock -> IO [PublicKey]
getTeamKeysForMicroblock db i aHash = do
  mb <- getKeyBlockByHash db i (Hash aHash)
  case mb of Nothing -> do
               -- writeLog aInfoChan [BDTag] Error ("No Team Keys For Key block " ++ show aHash)
               return []
             Just r -> return $ _teamKeys (r :: MacroblockBD)


tMicroblockBD2MicroblockAPI :: DBPoolDescriptor -> InChan InfoMsg -> MicroblockBD -> IO MicroblockAPI
tMicroblockBD2MicroblockAPI db i m@(MicroblockBD {..}) = do
  tx <- getTxsMicroblock db i m
  let txAPI = map (\t -> TransactionAPI {_tx = t, _txHash = rHashT t }) tx
  -- teamKeys <- getTeamKeysForMicroblock db _keyBlock --aInfoChan
  return MicroblockAPI {
            _prevMicroblock = Nothing,
            _nextMicroblock = Nothing,
            _keyBlock,
            _signAPI = _signBD,
            -- _teamKeys = teamKeys,
            _publisher,
            _transactionsAPI = txAPI
            }


tMacroblock2MacroblockAPI :: DBPoolDescriptor -> MacroblockBD -> IO MacroblockAPI
tMacroblock2MacroblockAPI descr (MacroblockBD {..}) = do
           microblocks <- zip _mblocks <$> mapM (\h -> getMicroBlockByHashDB descr (Hash h)) _mblocks
           let microblocksInfoAPI = map (\(h, MicroblockBD {..}) -> MicroblockInfoAPI {
                                                        _prevMicroblock = Nothing,
                                                        _nextMicroblock = Nothing,
                                                        _keyBlock,
                                                        _signAPI = _signBD,
                                                        _publisher,
                                                        _hash = h}) microblocks
           return $ MacroblockAPI {
             _prevKBlock,
             _nextKBlock = Nothing,
             _difficulty,
             _height,
             _solver,
             _reward,
             _mblocks = microblocksInfoAPI,
             _teamKeys }



dummyMacroblock :: MacroblockBD
dummyMacroblock = MacroblockBD {
  _prevKBlock = Nothing,
  _nextKBlock = Nothing,
  _prevHKBlock = Nothing,
  _difficulty = 0,
  _height = 0,
  _solver = aSolver,
  _reward = 0,
  _time = 0,
  _number = 0,
  _nonce = 0,
  _mblocks = [],
  _teamKeys = []
}
  where aSolver = read "1" :: PublicKey


tKeyBlockInfo2Macroblock :: KeyBlockInfo -> MacroblockBD
tKeyBlockInfo2Macroblock (KeyBlockInfo {..}) = MacroblockBD {
            _prevKBlock = Nothing,
            _nextKBlock = Nothing,
            _prevHKBlock = Just _prev_hash,
            _difficulty = 20,
            _height = 0,
            _solver,
            _reward = 0,
            _time,
            _number,
            _nonce,
            _mblocks = [],
            _teamKeys = []
          }

tMacroblock2KeyBlockInfo :: MacroblockBD -> KeyBlockInfo
tMacroblock2KeyBlockInfo (MacroblockBD {..}) = KeyBlockInfo {
  _time     ,
  _prev_hash = prev_hash,
  _number   ,
  _nonce    ,
  _solver   ,
  _type = 0}
  where prev_hash = case _prevHKBlock of
          Nothing -> ""
          Just j  -> j


tMacroblock2ChainInfo :: Maybe (DBKey, MacroblockBD) -> IO ChainInfo
tMacroblock2ChainInfo kv = do
  case kv of
    Nothing ->  return ChainInfo {
    _emission        = 0,
    _curr_difficulty = 0,
    _last_block      = "",
    _blocks_num      = 0,
    _txs_num         = 0,  -- quantity of all approved transactions
    _nodes_num       = 0   -- quantity of all active nodes
    }
    Just (aKeyBlockHash, (MacroblockBD {..}))  -> return ChainInfo {
    _emission        = _reward,
    _curr_difficulty = _difficulty,
    _last_block      = aKeyBlockHash,
    _blocks_num      = 0,
    _txs_num         = 0,  -- quantity of all approved transactions
    _nodes_num       = 0   -- quantity of all active nodes
    }


getKeyBlockHash :: KeyBlockInfoPoW -> BSI.ByteString
getKeyBlockHash  KeyBlockInfoPoW {..} = Base64.encode . SHA.hash $ bstr
  where bstr = B.concat $ map runPut [
                  putWord8    (toEnum _type)
               ,  putWord32le (fromInteger _number)
               ,  putWord32le (fromInteger _time)
               ,  putWord32le (fromInteger _nonce)
               ]  ++  [
                  fromRight "" $ Base64.decode _prev_hash
               ,  fromRight "" $ Base64.decode _solver
               ]


updateMacroblockByKeyBlock :: DBPoolDescriptor -> InChan InfoMsg -> HashOfKeyBlock -> KeyBlockInfo -> BranchOfChain -> IO ()
updateMacroblockByKeyBlock db i hashOfKeyBlock keyBlockInfo branch = do
    writeLog i [BDTag] Info $ "keyBlockInfo: " ++ show keyBlockInfo
    val <- getKeyBlockByHash db i (Hash hashOfKeyBlock)
    mb <- case val of
        Nothing -> return $ tKeyBlockInfo2Macroblock keyBlockInfo
        Just j  -> return $ fillMacroblockByKeyBlock j keyBlockInfo

    writeMacroblockToDB db i hashOfKeyBlock mb
    let aNumber = _number (keyBlockInfo :: KeyBlockInfo)
        mes = "going to write number " ++ show aNumber ++ show hashOfKeyBlock ++ show branch
    writeLog i [BDTag] Info mes
    setChain (Common db i ) aNumber hashOfKeyBlock branch
    writeKeyBlockNumber (Common db i) $ _number (keyBlockInfo :: KeyBlockInfo)


writeMacroblockToDB :: DBPoolDescriptor -> InChan InfoMsg -> HashOfKeyBlock -> MacroblockBD -> IO ()
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



writeKeyBlockNumber :: Common -> Number -> IO ()
writeKeyBlockNumber (Common descr _) aNumber= do
  let value = S.encode aNumber
  funW (poolSprout descr) [(lastKeyBlock, value)]


getKeyBlockNumber :: Common -> IO (Maybe Number)
getKeyBlockNumber c@(Common descr i) = do
  value <- funR (poolSprout descr) lastKeyBlock
  -- if Nothing write genesis KeyBlock
  case value of
    Nothing -> do
      let k = tKBIPoW2KBI genesisKeyBlock
          h = getKeyBlockHash genesisKeyBlock
          mes = "The first time in history, genesis kblock " ++ show h ++ show k
      writeLog i [BDTag] Info mes
      updateMacroblockByKeyBlock descr i h k Main
      writeLog i [BDTag] Info "Genesis block was written"
      getKeyBlockNumber c
    Just v  -> case S.decode v :: Either String Number of
      Left e  -> throw (DecodeException (show e))
      Right r -> return $ Just r


setChain :: Common -> Number -> HashOfKeyBlock -> BranchOfChain -> IO ()
setChain c@(Common descr i ) aNumber hashOfKeyBlock branch = do
  chain <- getChain c aNumber
  let valueOfChain = funBranch branch $ chain
  let newChain = if (valueOfChain == Nothing)
        then case branch of
        Main   -> (Just hashOfKeyBlock, snd chain)
        Sprout -> (fst chain, Just hashOfKeyBlock)
        else throw (ValueOfChainIsNotNothing ("KeyBlockHash is" ++ (show valueOfChain)))

  let key = S.encode aNumber
      val = S.encode (newChain :: Chain)
  funW (poolSprout descr) [(key, val)]
  writeLog i [BDTag] Info $ "Write number  " ++ show aNumber ++ show newChain ++ show branch


setChainAndDeleteOther :: Common -> Number -> HashOfKeyBlock -> BranchOfChain -> IO ()
setChainAndDeleteOther c@(Common descr i ) aNumber hashOfKeyBlock branch = do
  chain <- getChain c aNumber
  -- let valueOfChain = funBranch branch $ chain
  let newChain = case branch of
        Main   -> (Just hashOfKeyBlock, Nothing)
        Sprout -> (Nothing, Just hashOfKeyBlock)


  let key = S.encode aNumber
      val = S.encode (newChain :: Chain)
  funW (poolSprout descr) [(key, val)]
  writeLog i [BDTag] Info $ "Write number  " ++ show aNumber ++ show newChain ++ show branch


funBranch :: BranchOfChain -> (a, a) -> a
funBranch Main   = fst
funBranch Sprout = snd


getChain :: Common -> Number -> IO Chain
getChain (Common descr _ ) aNumber = do
  maybeV <- funR (poolSprout descr) (S.encode aNumber)
  case maybeV of
    Nothing    -> return (Nothing, Nothing)
    Just m -> case S.decode m :: Either String Chain of
      Left e  -> throw (DecodeException (show e))
      Right r -> return r


genesisKeyBlock :: KeyBlockInfoPoW
genesisKeyBlock = KeyBlockInfoPoW{
  _time = 0,
  _prev_hash = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=",
  _number = 0,
  _nonce = 0,
  _solver = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=",
  _type = 0}


tKBIPoW2KBI :: KeyBlockInfoPoW -> KeyBlockInfo
tKBIPoW2KBI (KeyBlockInfoPoW {..}) = KeyBlockInfo {
  _time,
  _prev_hash,
  _number,
  _nonce,
  _solver = pubKey,
  _type}
  where pubKey = publicKey256k1 ((roll $ B.unpack _solver) :: Integer)




fillMacroblockByKeyBlock :: MacroblockBD -> KeyBlockInfo -> MacroblockBD
fillMacroblockByKeyBlock m (KeyBlockInfo {..}) = m {
        _prevHKBlock = Just $ _prev_hash,
        _solver = _solver,
        _time = _time,
        _number = _number,
        _nonce  = _nonce}
