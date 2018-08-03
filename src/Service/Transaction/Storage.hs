{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Service.Transaction.Storage where

import           Control.Concurrent.Chan.Unagi.Bounded
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
import qualified Data.Map                              as Map
import           Data.Maybe
import           Data.Pool
import qualified Data.Serialize                        as S (encode)
import           Data.Serialize.Put
import qualified "rocksdb-haskell" Database.RocksDB    as Rocks
import           Node.Data.GlobalLoging
import           Service.InfoMsg                       (InfoMsg (..),
                                                        LogingTag (..),
                                                        MsgType (..))
import           Service.System.Directory
import           Service.Transaction.Decode
import           Service.Transaction.Iterator
import           Service.Transaction.Transformation
import           Service.Types
import           Service.Types.PublicPrivateKeyPair
import           Service.Types.SerializeJSON           ()


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
  poolLast        <- fun =<< getLastFilePath
  -- putStrLn "DBTransactionException"
  -- sleepMs 5000
  -- throw DBTransactionException
  let offsetMap = kvOffset
  return (DBPoolDescriptor poolTransaction poolMicroblock poolLedger poolMacroblock poolSprout poolLast offsetMap)


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
isMacroblockClosed MacroblockBD {..} _ = do
  -- writeLog i [BDTag] Info $ "checkMacroblockIsClosed: length _mblocks " ++ show (length _mblocks)
  -- writeLog i [BDTag] Info $ "checkMacroblockIsClosed: length _teamKeys" ++ show (length _teamKeys)
  return $ length _mblocks /= 0 && length _teamKeys == length _mblocks


getChainInfoDB :: Common -> IO ChainInfo
getChainInfoDB c = do
  kv <- getLastKeyBlock c
  tMacroblock2ChainInfo kv


getLastKeyBlock  :: Common -> IO (Maybe (DBKey,MacroblockBD))
getLastKeyBlock (Common desc aInfoChan) = do
  key <- funR (poolLast desc) lastClosedKeyBlock
  -- key <- getLastKeyBlockNumber (Common desc aInfoChan)
  case key of Nothing -> return Nothing
              Just k  -> do
                mb <- getKeyBlockByHash desc aInfoChan (Hash k)
                case mb of Nothing -> do
                                writeLog aInfoChan [BDTag] Error "No Key block "
                                return Nothing
                           Just r -> return $ Just (k,r)


getLastTransactions :: DBPoolDescriptor -> OffsetMap -> PublicKey -> Int -> Int -> IO [TransactionAPI]
getLastTransactions descr aOffsetMap pubKey offset aCount = do
  let fun db = getPartTransactions db aOffsetMap pubKey offset aCount
  withResource (poolTransaction descr) fun


getPartTransactions :: Rocks.DB -> OffsetMap -> PublicKey -> Int -> Int -> IO [TransactionAPI]
getPartTransactions db aOffsetMap pubKey offset aCount = do
  let key = (pubKey, offset)
      -- aOffsetMap = kvOffset
      aIt = Map.lookup key aOffsetMap
      fun iter count = nLastValues iter count (decodeAndFilter pubKey)
  case aIt of
    Nothing -> do
      it <- getLastIterator db
      decodeTx  =<< (drop offset <$> fun it (aCount + offset))
    Just it  -> do
      decodeTx  =<< (fun it aCount)


getTransactionsByMicroblockHash :: Common -> Hash -> IO (Maybe [TransactionInfo])
getTransactionsByMicroblockHash (Common db i) aHash = do
  m@MicroblockBD {..} <- getMicroBlockByHashDB db aHash
  txInfo <- getTxs db i m
  return $ Just txInfo


getBlockByHashDB :: Common -> Hash  -> IO (Maybe MicroblockAPI)
getBlockByHashDB (Common db i) hash = do
  m <- getMicroBlockByHashDB db hash
  mAPI <- tMicroblockBD2MicroblockAPI db i m --aInfoChan
  return $ Just mAPI


decodeTransactionsAndFilterByKey :: [DBValue] -> PublicKey -> [TransactionAPI]
decodeTransactionsAndFilterByKey rawTx pubKey = mapMaybe (decodeTransactionAndFilterByKey pubKey) rawTx


getKeyBlockByHashDB :: Common -> Hash  -> IO (Maybe MacroblockAPI)
getKeyBlockByHashDB (Common db i) kHash = do
  hashOfKey <- getKeyBlockByHash db i kHash
  case hashOfKey of Nothing -> return Nothing
                    Just j  -> Just <$> tMacroblock2MacroblockAPI db j


getAllTransactionsDB :: Common -> PublicKey -> IO [TransactionAPI]
getAllTransactionsDB (Common descr _) pubKey = do
  txByte <- withResource (poolTransaction descr) getAllValues
  return $ decodeTransactionsAndFilterByKey txByte pubKey


tMacroblock2MacroblockAPI :: DBPoolDescriptor -> MacroblockBD -> IO MacroblockAPI
tMacroblock2MacroblockAPI descr MacroblockBD {..} = do
           microblocks <- zip _mblocks <$> mapM (getMicroBlockByHashDB descr . Hash) _mblocks
           let microblocksInfoAPI = map (\(h, MicroblockBD {..}) -> MicroblockInfoAPI {
                                                        _prevMicroblock = Nothing,
                                                        _nextMicroblock = Nothing,
                                                        _keyBlock,
                                                        _signAPI = _signBD,
                                                        _publisher,
                                                        _hash = h}) microblocks
           return MacroblockAPI {
             _prevKBlock,
             _nextKBlock = Nothing,
             _difficulty,
             _height = _number,
             _solver,
             _reward,
             _mblocks = microblocksInfoAPI,
             _teamKeys }


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
updateMacroblockByKeyBlock (Common db i) hashOfKeyBlock keyBlockInfo branch = do
    writeLog i [BDTag] Info $ "keyBlockInfo: " ++ show keyBlockInfo
    val <- getKeyBlockByHash db i (Hash hashOfKeyBlock)
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
updateMacroblockByMacroblock (Common db i) hashOfKeyBlock mb  branch = do
    writeLog i [BDTag] Info $ "Macroblock: " ++ show mb
    getKeyBlockByHash db i (Hash hashOfKeyBlock) >>= \case
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
  hashPreviousLastKeyBlock <- funR (poolLast desc) lastClosedKeyBlock
  aIsMacroblockClosed <- isMacroblockClosed aMacroblock a
  let cMacroblock = if aIsMacroblockClosed
        then (aMacroblock { _prevKBlock = hashPreviousLastKeyBlock }) :: MacroblockBD
        else aMacroblock
  print $ "cMacroblock" ++ show cMacroblock
  let cKey = hashOfKeyBlock
      cVal = S.encode cMacroblock
  print $ "cKey " ++ show cKey
  print $ "cVal " ++ show cVal
  funW (poolMacroblock desc) [(cKey,cVal)]
  writeLog a [BDTag] Info ("Write Macroblock " ++ show cKey ++ " " ++ show cMacroblock ++ "to DB")

  -- For closed Macroblock
  bIsMacroblockClosed <- isMacroblockClosed aMacroblock a
  when bIsMacroblockClosed $ do
    -- fill _nextKBlock for previous closed Macroblock
    bdKV <- case hashPreviousLastKeyBlock of
      Nothing -> return []
      Just j  -> do
        previousLastKeyBlock <- funR (poolMacroblock desc) j
        case previousLastKeyBlock of
          Nothing -> return []
          Just k -> do
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
  _solver = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=",
  _type = 0}
