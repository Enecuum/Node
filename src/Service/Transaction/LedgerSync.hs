{-# LANGUAGE DuplicateRecordFields #-}

module Service.Transaction.LedgerSync where

import           Control.Exception
import           Control.Monad                    (forM)
import           Data.Maybe
import qualified Data.Serialize                   as S (encode)
import           Node.Data.GlobalLoging
import           Service.InfoMsg                  (LogingTag (..), MsgType (..))
import           Service.Transaction.Balance
import           Service.Transaction.Independent
import           Service.Transaction.Sprout
import           Service.Transaction.SproutCommon
import           Service.Transaction.Storage
import           Service.Types


myTail ::  Common -> IO (Number, HashOfKeyBlock)
myTail (Common descr i) = do
  kv <- getLastKeyBlock descr i
  -- kv <- getKeyBlockNumber (Common descr i)
  case kv of
    Nothing -> throw NoLastKeyBlock
    Just (hashOfKeyBlock, mb)  -> do
      let n =  _number (mb :: MacroblockBD)
      return (n, hashOfKeyBlock)


peekNPreviousKeyBlocks :: Common -> From -> To -> IO [(Number, HashOfKeyBlock)]
peekNPreviousKeyBlocks c from to = do
  let numbers = [from .. to]
  vM <- mapM (\n -> getM c n) numbers
  let kvM = zip numbers vM
      kvJust = filter (\(_,v) -> v /= Nothing) kvM
      kv = map (\(k,v) -> (k, fromJust v)) kvJust
  return kv


getKeyBlockSproutData :: Common -> From -> To -> IO [(Number, HashOfKeyBlock, MacroblockBD)]
getKeyBlockSproutData c@(Common descr i) from to = do
  kv <- peekNPreviousKeyBlocks c from to
  mb <- mapM (\(_,aHash) -> getKeyBlockByHash descr (Hash aHash) i) kv
  let allMaybe = zipWith (\(aNumber, aHash) aMacroblock -> (aNumber, aHash, aMacroblock)) kv mb
      allJust = filter (\(_,_,v) -> v /= Nothing) allMaybe
      allKeyData = map (\(n,h,m) -> (n, h, fromJust m)) allJust
  return allKeyData


isValidKeyBlockSprout :: Common -> [(HashOfKeyBlock, MacroblockBD)] -> IO Bool
isValidKeyBlockSprout _ kv = do --return True  --return $ h == _prevHKBlock m
  -- hash of Key Block is real hash
  let fun h m = (h ==) $ getKeyBlockHash $ tKeyBlockToPoWType $ tMacroblock2KeyBlockInfo m
      isRealHash = map (\(h,m) -> fun h m) kv
  return (and isRealHash)


setKeyBlockSproutData :: Common -> [(HashOfKeyBlock,MacroblockBD)] -> IO ()
setKeyBlockSproutData c@(Common descr i) kv = do
  mapM_ (\(h,m) -> updateMacroblockByKeyBlock descr i h (tMacroblock2KeyBlockInfo m) Sprout) kv

  -- read from and write to Sprout Table
  let kvN = map (\(h,m) -> (h, _number (m :: MacroblockBD))) kv
  mapM_ (\(hashOfKeyBlock, aNumber) -> setChain c aNumber hashOfKeyBlock Sprout) kvN


getRestSproutData :: Common -> HashOfMicroblock -> IO MicroBlockContent
getRestSproutData (Common descr i) hashOfMicroblock = do
  microblock <- getMicroBlockByHashDB descr (Hash hashOfMicroblock)
  -- case microblock of Nothing -> throw NoSuchMicroBlockDB
  --                    Just m -> do
  tx <- getTransactionsByMicroblockHash descr i (Hash hashOfMicroblock)
  case tx of Nothing -> throw NoSuchTransactionDB
             Just t  -> return $ MicroBlockContent microblock t


isValidRestOfSprout :: Common -> MicroBlockContent -> IO Bool
isValidRestOfSprout _ _ = do -- Fix verify transaction signature
  return True


setRestSproutData :: Common -> (Number, HashOfKeyBlock, MicroBlockContent) -> IO ()
setRestSproutData c@(Common descr i) (aNumber, hashOfKeyBlock, (MicroBlockContent mb txInfo )) = do
  -- write MicroBlockContent MicroblockBD [TransactionInfo]
  let tx = map (\t -> _tx (t :: TransactionInfo)) txInfo
  writeTransactionDB descr i tx (rHash mb)
  writeMicroblockDB descr i mb

  -- add hashes of microblocks to Macroblock table
  addMicroblockHashesToMacroBlock descr i hashOfKeyBlock [rHash mb]
  -- write number and hashOfKeyBlock to Sprout table
  setChain c aNumber hashOfKeyBlock Sprout


deleteSproutData      :: Common -> Number -> IO () -- right after foundation
deleteSproutData c aNumber = do
  let branch = Sprout
  chain <- findWholeChainSince c aNumber branch
  mapM_ (\r -> deleteSprout c r branch) chain


deleteSprout :: Common -> (Number, HashOfKeyBlock) -> BranchOfChain -> IO () -- right after foundation
deleteSprout c@(Common descr i) (aNumber, hashOfKeyBlock) branch = do
  macroblock <- getKeyBlockByHash descr (Hash hashOfKeyBlock) i
  case macroblock of
    Nothing -> writeLog i [BDTag] Error ("There is no KeyBlock "  ++ show hashOfKeyBlock)
    Just m -> do
      let hashesOfMicroBlocks = _mblocks (m :: MacroblockBD)
      microblocks <- mapM (\h -> getMicroBlockByHashDB descr (Hash h)) hashesOfMicroBlocks
      let hashesOfTransactions = concat $ map _transactionsHashes microblocks
      -- delete Transactions
      mapM_ (funD (poolTransaction descr)) hashesOfTransactions
      -- delete MicroBlocks
      mapM_ (funD (poolMicroblock descr)) hashesOfMicroBlocks
      -- delete KeyBlock
      funD (poolMacroblock descr) hashOfKeyBlock
      -- erase chain from Sprout table
      (aMain,aSprout) <- getChain c aNumber
      let newChain = case branch of
            Main   -> (Nothing, aSprout)
            Sprout ->  (aMain, Nothing)
          sKey   = S.encode aNumber
          sValue = S.encode newChain
      funW (poolSprout descr) [(sKey, sValue)]


setSproutAsMain :: Common -> Number -> IO () -- right after foundation
setSproutAsMain c@(Common descr i) aNumber = do
  -- find key blocks which belong to Main chain (right after foundation of main and sprout chain)
  mainChain <- findWholeChainSince c aNumber Main
  mainChainClosedKeyBlocks <- getClosedMacroblockByHash c $ map snd mainChain
  sproutChain <- findWholeChainSince c aNumber Sprout
  sproutChainClosedKeyBlocks <- getClosedMacroblockByHash c $ map snd sproutChain
  -- recalculate ledger
  -- storno
  mapM_ (\(h,m) -> calculateLedger descr i True h m) mainChainClosedKeyBlocks
  -- add closed sprout macroblocks to ledger
  mapM_ (\(h,m) -> calculateLedger descr i False h m) sproutChainClosedKeyBlocks
  -- delete Main chain (right after foundation of main and sprout chain)
  mapM_ (\r -> deleteSprout c r Sprout) mainChain
  -- set SproutChain as MainChain
  mapM_ (\(n,h) -> setChain c n h Main) sproutChain
  let lastNumber = fst $ last sproutChain
  writeKeyBlockNumber c lastNumber


-- get all closed macroblocks for calculating ledger
getClosedMacroblockByHash :: Common -> [HashOfKeyBlock] -> IO [(HashOfKeyBlock, MacroblockBD)]
getClosedMacroblockByHash co hashesOfKeyBlock = do
  macroblocks <- forM hashesOfKeyBlock $ findClosedMacroblocks co
  return $ map fromJust $ filter (/= Nothing ) macroblocks
  where
        findClosedMacroblocks :: Common -> HashOfKeyBlock -> IO (Maybe (HashOfKeyBlock, MacroblockBD))
        findClosedMacroblocks (Common descr i) h = do
          macroblockMaybe <- getKeyBlockByHash descr (Hash h) i
          case macroblockMaybe of
            Nothing -> throw (NoClosedKeyBlockInDB (show h))
            Just j  -> if (checkMacroblockIsClosed j)
              then return $ Just (h, j)
              else return Nothing
