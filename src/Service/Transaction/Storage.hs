{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-orphans     #-}

module Service.Transaction.Storage where

-- import           Control.Applicative
import           Control.Concurrent.Chan.Unagi.Bounded
--import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad                         (when)
import qualified Control.Monad.Catch                   as E
import           Control.Retry
import qualified Crypto.Hash.SHA256                    as SHA
import qualified Data.ByteString                       as B
import qualified Data.ByteString.Base64                as Base64
import qualified Data.ByteString.Internal              as BSI
import           Data.Default                          (def)
import           Data.Either
--import qualified Data.Map                              as Map
import           Data.Maybe
import           Data.Pool
import qualified Data.Serialize                        as S (encode)
import           Data.Serialize.Put
import qualified "rocksdb-haskell" Database.RocksDB    as Rocks
import           Node.Data.GlobalLoging
--import           Node.DataActor
--import           Service.Chan
import           Service.InfoMsg                       (InfoMsg (..),
                                                        LogingTag (..),
                                                        MsgType (..))
import           Service.System.Directory
import           Service.Transaction.Decode
import           Service.Transaction.Iterator
import           Service.Transaction.Sprout
import           Service.Transaction.Transformation
import           Service.Types
import           Service.Types.PublicPrivateKeyPair
import           Service.Types.SerializeJSON           ()

-- FIX change def (5 times)
connectOrRecoveryConnect :: IO DBPoolDescriptor
connectOrRecoveryConnect = recovering def handler . const $ connectDB


allDB :: IO [FilePath]
allDB = do
  transactionFilePath <- getTransactionFilePath
  microblockFilePath <- getMicroblockFilePath
  ledgerFilePath <- getLedgerFilePath
  macroblockFilePath <- getMacroblockFilePath
  sproutFilePath <- getSproutFilePath
  lastFilePath <- getLastFilePath
  return [transactionFilePath, microblockFilePath, ledgerFilePath, macroblockFilePath, sproutFilePath, lastFilePath]


connectDB :: IO DBPoolDescriptor
connectDB = do
  let fun dbFilePath = createPool (Rocks.open dbFilePath def{Rocks.createIfMissing=True}) Rocks.close 1 32 16
  poolTransaction <- fun =<< getTransactionFilePath
  poolMicroblock  <- fun =<< getMicroblockFilePath
  poolLedger      <- fun =<< getLedgerFilePath
  poolMacroblock  <- fun =<< getMacroblockFilePath
  poolSprout      <- fun =<< getSproutFilePath
  poolLast        <- fun =<< getLastFilePath
  -- putStrLn "DBTransactionException"
  -- sleepMs 5000
  -- throw DBTransactionException
  return (DBPoolDescriptor poolTransaction poolMicroblock poolLedger poolMacroblock poolSprout poolLast)


data SuperException = DBTransactionException
                    | NotImplementedException -- test
                    | OtherException
                  deriving (Show)

instance Exception SuperException


--catch all exceptions and retry connections
handler :: [p -> E.Handler IO Bool]
handler = [ \_ -> E.Handler $ \(_ :: SomeException) -> return True ]

-- End of the Connection section
--------------------------------------



lastClosedKeyBlock :: DBKey
lastClosedKeyBlock = "2dJ6lb9JgyQRac0DAkoqmYmS6ats3tND0gKMLW6x2x8=" :: DBKey



bdLog :: InChan InfoMsg -> String -> IO ()
bdLog i msg = writeLog i [BDTag] Info msg

bsLog :: Show a => InChan InfoMsg -> a -> IO ()
bsLog i msg = writeLog i [BDTag] Info $ show msg


isMacroblockClosed :: MacroblockBD -> InChan InfoMsg -> IO Bool
isMacroblockClosed MacroblockBD {..} _ = return $ not (null _mblocks) && length _teamKeys == length _mblocks
  -- writeLog i [BDTag] Info $ "checkMacroblockIsClosed: length _mblocks " ++ show (length _mblocks)
  -- writeLog i [BDTag] Info $ "checkMacroblockIsClosed: length _teamKeys" ++ show (length _teamKeys)



getChainInfoDB :: Common -> IO ChainInfo
getChainInfoDB c = tMacroblock2ChainInfo =<< getLastKeyBlock c



getLastKeyBlock  :: Common -> IO (Maybe (DBKey,MacroblockBD))
getLastKeyBlock c@(Common desc aInfoChan) = do
  key <- funR (poolLast desc) lastClosedKeyBlock
  -- key <- getLastKeyBlockNumber (Common desc aInfoChan)
  case key of Nothing -> return Nothing
              Just k  -> do
                mb <- getKeyBlockByHash c (Hash k)
                case mb of Nothing -> do
                                writeLog aInfoChan [BDTag] Error "No Key block "
                                return Nothing
                           Just r -> return $ Just (k,r)


getLastTransactions :: Common -> InContainerChan -> PublicKey -> Int -> Int -> IO [TransactionAPI]
getLastTransactions (Common descr i) aOffsetMap pubKey offset aCount = do
  let fun db = getPartTransactions db i aOffsetMap pubKey offset aCount
  withResource (poolTransaction descr) fun

{-
Predicate :: (m a -> Bool)    -> MVar Bool      -> ContainerCommands m a
Lookup    :: (m a -> Maybe a) -> MVar (Maybe a) -> ContainerCommands m a
-- insert, delete, adjust, updateWithKey
Update    :: (m a -> m a)                       -> ContainerCommands m a
-- size, length
Size      :: (m a -> Int)     -> MVar Int       -> ContainerCommands m a

-}



getPartTransactions :: Rocks.DB -> InChan InfoMsg -> InContainerChan -> PublicKey -> Int -> Int -> IO [TransactionAPI]
getPartTransactions db i _ pubKey offset aCount = do
  let key = (pubKey, offset)

  -- aVar <- newEmptyMVar
  -- writeInChan inContainerChan (Lookup (Map.lookup key) aVar)
  -- aIt <- takeMVar aVar
  let aIt = Nothing
  (realCount, realOffset, startIt) <- case aIt of
    Nothing -> do
      bdLog i "Going to find last iterator"
      it <- Rocks.createIter db  Rocks.defaultReadOptions
      Rocks.iterLast it
      return (aCount, offset, it)
    Just it  -> do
      bdLog i $ "We have found non empty iterator for key : " ++ show key
      isValid <- Rocks.iterValid it
      if isValid
        then do
        bdLog i $ "Iterator is still valid, we will use it"
        return (aCount - offset, 0, it)
        else do
        bdLog i $ "Iterator is already invalid, we will create a new one"
        newIt <- Rocks.createIter db  Rocks.defaultReadOptions
        Rocks.iterLast newIt
        return (aCount + offset, offset, newIt)

  bdLog i $ "realCount " ++ show realCount
  bdLog i $ "realOffset " ++ show realOffset
  isValid <- Rocks.iterValid startIt
  print =<< (("isIterValid:" ++) . show <$> Rocks.iterValid startIt)
  if isValid
    then do
    (values, newIter) <- nLastValues startIt realCount (decodeAndFilter pubKey)
    print $ "newIter == startIt :" ++ show ( newIter == startIt)
    print values
    txAPI <- decodeTx $ drop realOffset values
    -- when (not $ null txAPI) $ do
    --   let newKey = (pubKey, realOffset + realCount)
    --   Rocks.iterNext newIter
    --   writeInChan inContainerChan (Update (Map.insert newKey newIter))
    --   print $ "save iterator" ++ show newKey ++ show newIter
    Rocks.releaseIter newIter
    return txAPI
    else do
    writeLog i [BDTag] Info "There are no valid iterator"
    return []


instance Show Rocks.Iterator where
  show _ = "Iter"


getTransactionsByMicroblockHash :: Common -> Hash -> IO (Maybe [TransactionInfo])
getTransactionsByMicroblockHash (Common db i) aHash = Just <$> (getTxs db i =<< getMicroBlockByHashDB db aHash)


getBlockByHashDB :: Common -> Hash  -> IO (Maybe MicroblockAPI)
getBlockByHashDB (Common db i) hash = Just <$> (tMicroblockBD2MicroblockAPI db i =<< getMicroBlockByHashDB db hash)


decodeTransactionsAndFilterByKey :: [DBValue] -> PublicKey -> [TransactionAPI]
decodeTransactionsAndFilterByKey rawTx pubKey = mapMaybe (decodeTransactionAndFilterByKey pubKey) rawTx


getKeyBlockByHashDB :: Common -> Hash  -> IO (Maybe MacroblockAPI)
getKeyBlockByHashDB c@(Common db _) kHash = do
  hashOfKey <- getKeyBlockByHash c kHash
  case hashOfKey of Nothing -> return Nothing
                    Just j  -> Just <$> tMacroblock2MacroblockAPI db j


getAllTransactionsDB :: Common -> PublicKey -> IO [TransactionAPI]
getAllTransactionsDB (Common descr _) pubKey = do
  txByte <- withResource (poolTransaction descr) getAllValues
  return $ decodeTransactionsAndFilterByKey txByte pubKey


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


updateMacroblockByKeyBlock :: Common -> HashOfKeyBlock -> KeyBlockInfo -> BranchOfChain -> IO ()
updateMacroblockByKeyBlock c@(Common db i) hashOfKeyBlock keyBlockInfo branch = do
    writeLog i [BDTag] Info $ "keyBlockInfo: " ++ show keyBlockInfo
    val <- getKeyBlockByHash c (Hash hashOfKeyBlock)
    mb <- case val of
        Nothing -> return $ tKeyBlockInfo2Macroblock keyBlockInfo
        Just j  -> return $ fillMacroblockByKeyBlock j keyBlockInfo

    writeMacroblockToDB (Common db i) hashOfKeyBlock mb
    let aNumber = _number (keyBlockInfo :: KeyBlockInfo)
        mes = "going to write number " ++ show aNumber ++ show hashOfKeyBlock ++ show branch
    writeLog i [BDTag] Info mes
    setChain (Common db i ) aNumber hashOfKeyBlock branch
    writeKeyBlockNumber (Common db i) $ _number (keyBlockInfo :: KeyBlockInfo)


updateMacroblockByMacroblock :: Common -> HashOfKeyBlock -> MacroblockBD -> BranchOfChain -> IO ()
updateMacroblockByMacroblock c@(Common db i) hashOfKeyBlock mb  branch = do
    writeLog i [BDTag] Info $ "Macroblock: " ++ show mb
    getKeyBlockByHash c (Hash hashOfKeyBlock) >>= \case
        Just _  -> writeLog i [BDTag] Warning $ "Macroblock with hash " ++ show hashOfKeyBlock ++ "is already in the table"
        Nothing -> do
            -- writeMacroblockToDB db i hashOfKeyBlock mb
            writeLog i [BDTag] Info $ "going to write number " ++ show (_number (mb :: MacroblockBD)) ++ show hashOfKeyBlock ++ show branch
            writeMacroblockSprout db i hashOfKeyBlock mb
            setChain (Common db i) (_number (mb :: MacroblockBD)) hashOfKeyBlock branch
            -- writeKeyBlockNumber (Common db i) $ _number (mb  :: MacroblockBD)


writeMacroblockSprout :: DBPoolDescriptor -> InChan InfoMsg -> HashOfKeyBlock -> MacroblockBD -> IO ()
writeMacroblockSprout desc a hashOfKeyBlock aMacroblock = do
    let cKey = hashOfKeyBlock
        cVal = S.encode aMacroblock
    funW (poolMacroblock desc) [(cKey,cVal)]
    writeLog a [BDTag] Info ("Write Macroblock " ++ show cKey ++ " " ++ show aMacroblock ++ "to DB")


writeMacroblockToDB :: Common -> HashOfKeyBlock -> MacroblockBD -> IO ()
writeMacroblockToDB (Common desc a) hashOfKeyBlock aMacroblock = do
  hashPreviousLastClosedKeyBlock <- funR (poolLast desc) lastClosedKeyBlock
  aIsMacroblockClosed <- isMacroblockClosed aMacroblock a
  let cMacroblock = if aIsMacroblockClosed
        then (aMacroblock { _prevKBlock = hashPreviousLastClosedKeyBlock }) :: MacroblockBD
        else aMacroblock
  print $ "cMacroblock" ++ show cMacroblock
  let cKey = hashOfKeyBlock
      cVal = S.encode cMacroblock
  print $ "cKey " ++ show cKey
  print $ "cVal " ++ show cVal
  funW (poolMacroblock desc) [(cKey,cVal)]
  writeLog a [BDTag] Info ("Write Macroblock " ++ show cKey ++ " " ++ show cMacroblock ++ "to DB")

  -- For closed Macroblock
  when aIsMacroblockClosed $ do
    writeLog a [BDTag] Info "going to fill _nextKBlock"
    -- fill _nextKBlock for previous closed Macroblock
    bdKV <- case hashPreviousLastClosedKeyBlock of
      Nothing -> return []
      Just j  -> do
        previousLastClosedKeyBlock <- funR (poolMacroblock desc) j
        case previousLastClosedKeyBlock of
          Nothing -> return []
          Just k -> do
            writeLog a [BDTag] Info $ "fill _nextKBlock: key " ++ show j ++ " _nextKBlock = " ++ show hashOfKeyBlock
            let r = decodeThis "MacroblockBD" k
                pKey = j
                pVal = S.encode (r { _nextKBlock = Just hashOfKeyBlock } :: MacroblockBD)
            return [(pKey, pVal)]
    -- fill new last closed Macroblock
    let keyValue = [(lastClosedKeyBlock, cKey)]
    funW (poolLast desc) keyValue
    funW (poolMacroblock desc) bdKV
    writeLog a [BDTag] Info ("Write Last Closed Macroblock " ++ show lastClosedKeyBlock ++ "to DB")



writeKeyBlockNumber :: Common -> Number -> IO ()
writeKeyBlockNumber (Common descr _) aNumber = funW (poolLast descr) [(lastKeyBlock, S.encode aNumber)]

-- ok
getKeyBlockNumber :: Common -> IO (Maybe Number)
getKeyBlockNumber c@(Common _ i) = do
  value <- getLastKeyBlockNumber c
  -- if Nothing write genesis KeyBlock
  case value of
    Nothing -> do
      -- Write genesis block
      let k = tKBIPoW2KBI genesisKeyBlock
          h = getKeyBlockHash genesisKeyBlock
      writeLog i [BDTag] Info $ "The first time in history, genesis kblock " ++ show h ++ show k
      updateMacroblockByKeyBlock c h k Main
      writeKeyBlockNumber c 0
      writeLog i [BDTag] Info "Genesis block was written"
      getKeyBlockNumber c
    Just v  -> return $ Just v


setChain :: Common -> Number -> HashOfKeyBlock -> BranchOfChain -> IO ()
setChain c@(Common descr i ) aNumber hashOfKeyBlock branch = do
  chain <- getChain c aNumber
  -- let valueOfChain = funBranch branch $ chain
  let newChain = case branch of
        -- if (valueOfChain == Nothing) then
        Main   -> (Just hashOfKeyBlock, snd chain)
        Sprout -> (fst chain, Just hashOfKeyBlock)
        -- else throw (ValueOfChainIsNotNothing ("KeyBlockHash is" ++ (show valueOfChain)))

  let key = S.encode aNumber
      val = S.encode (newChain :: Chain)
  funW (poolSprout descr) [(key, val)]
  writeLog i [BDTag] Info $ "Write number  " ++ show aNumber ++ show newChain ++ show branch


setChainAndDeleteOther :: Common -> Number -> HashOfKeyBlock -> BranchOfChain -> IO ()
setChainAndDeleteOther (Common descr i ) aNumber hashOfKeyBlock branch = do
  -- chain <- getChain c aNumber
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



genesisKeyBlock :: KeyBlockInfoPoW
genesisKeyBlock = KeyBlockInfoPoW{
  _time = 0,
  _prev_hash = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=",
  _number = 0,
  _nonce = 0,
  _solver = "EMde81cgGToGrGWSNCqm6Y498qBpjEzRczBbvC5MV2Q=",
  _type = 0}

g2 :: HashOfKeyBlock
g2 = getKeyBlockHash genesisKeyBlock
-- g3 = read "4z9ADFAWehl6XGW2/N+2keOgNR921st3oPSVxv08hTY=" :: HashOfKeyBlock


type NumberOfKeyBlock = Integer


getMickroblocks :: Common -> NumberOfKeyBlock -> IO [Microblock]
getMickroblocks c@(Common db i) kNumber = do
  m <- getKeyBlockMain c kNumber
  let microblocksHashes = map Hash $ _mblocks (m :: MacroblockBD)
      fun h = tMicroblockBD2Microblock db i =<< getMicroBlockByHashDB db h
  mapM fun microblocksHashes


getKeyBlockMain :: Common -> NumberOfKeyBlock-> IO MacroblockBD
getKeyBlockMain c kNumber = do
  (_, hashMaybe) <- findChain c kNumber Main
  case hashMaybe of
    Nothing -> throw NoSuchKBlockDB
    Just kHash -> do
      mb <- getKeyBlockByHash c (Hash kHash)
      case mb of
        Nothing -> throw NoSuchKBlockDB
        Just m  -> return m


getKeyBlock :: Common -> NumberOfKeyBlock-> IO KeyBlockInfoPoW
getKeyBlock c kNumber = tKeyBlockToPoWType . tMacroblock2KeyBlockInfo <$> getKeyBlockMain c kNumber
