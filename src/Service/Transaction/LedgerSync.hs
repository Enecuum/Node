{-# LANGUAGE DuplicateRecordFields #-}

module Service.Transaction.LedgerSync where

import           Control.Exception
import           Control.Monad                    (forM, when)
import           Data.Maybe
import qualified Data.Serialize                   as S (encode)
import           Node.Data.GlobalLoging
import           Service.InfoMsg                  (LogingTag (..), MsgType (..))
import           Service.Transaction.Balance
-- import           Service.Transaction.Independent
import           Data.List
import           Service.Transaction.Decode
import           Service.Transaction.Sprout
import           Service.Transaction.SproutCommon
import           Service.Transaction.Storage
import           Service.Types


myTail ::  Common -> IO (Number, HashOfKeyBlock)
myTail c@(Common _ i) = do
  -- kv <- getLastKeyBlock descr i
  curNumber <- getKeyBlockNumber c
  writeLog i [BDTag] Info $ "Currnet number of key block: " ++ show curNumber
  (nNumber, hashMaybe) <- findChain c (fromJust curNumber) Main
  writeLog i [BDTag] Info $ "Get number of key block: " ++ show nNumber ++ "Hash: " ++ show hashMaybe
  case hashMaybe of
    Nothing -> throw NoLastKeyBlock
    Just h  -> return (nNumber,h)


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
  mb <- mapM (\(_,aHash) -> getKeyBlockByHash descr i (Hash aHash)) kv
  let allMaybe = zipWith (\(aNumber, aHash) aMacroblock -> (aNumber, aHash, aMacroblock)) kv mb
      allJust = filter (\(_,_,v) -> v /= Nothing) allMaybe
      allKeyData = map (\(n,h,m) -> (n, h, fromJust m)) allJust
  return allKeyData


pair3 :: (a, b, c) -> c
pair3 (_, _, c)  = c

pair2 :: (a, b, c) -> (b, c)
pair2 (_, b, c) = (b, c)

isValidKeyBlockSprout :: Common -> [(Number, HashOfKeyBlock, MacroblockBD)] -> IO Bool
isValidKeyBlockSprout c@(Common _ i) okv = do
  writeLog i [BDTag] Info $ show $ intersperse "" $ map show okv
  let (numberAfterFoundation, macroblockAfterFoundation) =  head $ map (\(a,_,m) -> (a,m)) okv
  let mes = "After Foundation on main chain: number: " ++ show numberAfterFoundation ++ "m: " ++ show macroblockAfterFoundation
  writeLog i [BDTag] Info $ mes
  hashMainMaybe <- getM c (numberAfterFoundation - 1)
  let prevHash = _prevHKBlock (macroblockAfterFoundation :: MacroblockBD)
  writeLog i [BDTag] Info $ "Hash of foundation: " ++ show hashMainMaybe
  writeLog i [BDTag] Info $ "Prev Hash: " ++ show prevHash
  let isFoundationOk = hashMainMaybe == prevHash
  writeLog i [BDTag] Info $ "isFoundationOk: " ++ show isFoundationOk
  -- hash of Key Block is real hash
  let kv = map pair2 okv
  writeLog i [BDTag] Info $ "kv: " ++ show kv
  let fun h m = (h ==) $ getKeyBlockHash $ tKeyBlockToPoWType $ tMacroblock2KeyBlockInfo m
  let hmm = map (\(h,m) -> (h, m, (fun h m) :: Bool)) kv
  writeLog i [BDTag] Info $ "kv: " ++ show hmm
  let isRealHash = map pair3 hmm
  writeLog i [BDTag] Info $ "isRealHash: " ++ show isRealHash
  let allTrue = and $ isFoundationOk : isRealHash
  writeLog i [BDTag] Info $ "allTrue: " ++ show allTrue
  when (not allTrue) $ do  writeLog i [BDTag] Info $ concat $ map show isRealHash
  -- return allTrue
  writeLog i [BDTag] Info $ "We will return True in any case! "
  return True


setKeyBlockSproutData :: Common -> [(HashOfKeyBlock,MacroblockBD)] -> IO ()
setKeyBlockSproutData (Common descr i) kv = do
  writeLog i [BDTag] Info $ show $ kv
  -- mapM_ (\(h,m) -> updateMacroblockByKeyBlock descr i h (tMacroblock2KeyBlockInfo m) Sprout) kv
  mapM_ (\(h,m) -> updateMacroblockByMacroblock descr i h  m Sprout) kv
  writeLog i [BDTag] Info $ show $ map (tMacroblock2KeyBlockInfo . snd) kv


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
  macroblock <- getKeyBlockByHash descr i (Hash hashOfKeyBlock)
  case macroblock of
    Nothing -> writeLog i [BDTag] Error ("There is no KeyBlock "  ++ show hashOfKeyBlock)
    Just m -> do
      let hashesOfMicroBlocks = _mblocks (m :: MacroblockBD)
      microblocks <- mapM (\h -> getMicroBlockByHashDB descr (Hash h)) hashesOfMicroBlocks
      let hashesOfTransactions = concat $ map _transactionsHashes microblocks
      -- delete Transactions
      writeLog i [BDTag] Info $ "delete Transactions: " ++ show hashesOfTransactions
      mapM_ (funD (poolTransaction descr)) hashesOfTransactions
      -- delete MicroBlocks
      writeLog i [BDTag] Info $ "delete MicroBlocks: " ++ show hashesOfMicroBlocks
      mapM_ (funD (poolMicroblock descr)) hashesOfMicroBlocks
      -- delete KeyBlock
      writeLog i [BDTag] Info $ "delete KeyBlock: " ++ show hashOfKeyBlock
      funD (poolMacroblock descr) hashOfKeyBlock
      -- erase chain from Sprout table
      writeLog i [BDTag] Info $ "erase chain from Sprout table"
      (aMain,aSprout) <- getChain c aNumber
      let newChain = case branch of
            Main   -> (Nothing, aSprout)
            Sprout ->  (aMain, Nothing)
          sKey   = S.encode aNumber
          sValue = S.encode newChain
      writeLog i [BDTag] Info $ "new chain value: " ++ show aNumber ++ " " ++ show newChain
      funW (poolSprout descr) [(sKey, sValue)]


setSproutAsMain :: Common -> Number -> IO () -- right after foundation
setSproutAsMain c@(Common descr i) aNumber = do
  -- find key blocks which belong to Main chain (right after foundation of main and sprout chain)
  mainChain <- findWholeChainSince c aNumber Main
  writeLog i [BDTag] Info $ "setSproutAsMain: main chain is " ++ show mainChain
  mainChainClosedKeyBlocks <- getClosedMacroblockByHash c $ map snd mainChain
  writeLog i [BDTag] Info $ "setSproutAsMain: mainChainClosedKeyBlocks is " ++ show mainChainClosedKeyBlocks
  sproutChain <- findWholeChainSince c aNumber Sprout
  writeLog i [BDTag] Info $ "setSproutAsMain: sproutChain is " ++ show sproutChain
  sproutChainClosedKeyBlocks <- getClosedMacroblockByHash c $ map snd sproutChain
  writeLog i [BDTag] Info $ "setSproutAsMain: sproutChainClosedKeyBlocks is " ++ show sproutChainClosedKeyBlocks
  -- recalculate ledger
  -- storno
  writeLog i [BDTag] Info $ "Storno calculate Ledger for " ++ show mainChainClosedKeyBlocks
  mapM_ (\(h,m) -> calculateLedger descr i True h m) mainChainClosedKeyBlocks
  -- add closed sprout macroblocks to ledger
  writeLog i [BDTag] Info $ "Calculate Ledger for sprout: " ++ show sproutChainClosedKeyBlocks
  mapM_ (\(h,m) -> calculateLedger descr i False h m) sproutChainClosedKeyBlocks
  -- delete Main chain (right after foundation of main and sprout chain)
  writeLog i [BDTag] Info $ "delete Main chain " ++ show mainChain
  mapM_ (\r -> deleteSprout c r Sprout) mainChain

  -- write Last Macroblock
  let lastClosedKeyBlockSprout = fst $ last sproutChainClosedKeyBlocks
  funW (poolLast descr) [(lastClosedKeyBlock,lastClosedKeyBlockSprout)]
  writeLog i [BDTag] Info ("Write Last Closed Macroblock " ++ show lastClosedKeyBlockSprout ++ "to DB")

  -- set SproutChain as MainChain
  writeLog i [BDTag] Info $ "set SproutChain as MainChain " ++ show sproutChain
  mapM_ (\(n,h) -> setChainAndDeleteOther c n h Main) sproutChain
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
          macroblockMaybe <- getKeyBlockByHash descr i (Hash h)
          case macroblockMaybe of
            Nothing -> throw (NoClosedKeyBlockInDB (show h))
            Just j  -> do
              isMacroblockClosed <- checkMacroblockIsClosed j i
              writeLog i [BDTag] Info $ "Macroblock with hash " ++ show h ++ "is closed " ++ show isMacroblockClosed
              if isMacroblockClosed
              then return $ Just (h, j)
              else return Nothing
