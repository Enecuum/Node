{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Service.Transaction.LedgerSync where

import           Control.Exception
import           Control.Monad                         (forM, when)
import           Data.Maybe
import qualified Data.Serialize                        as S (encode)
import           Node.Data.GlobalLoging
import           Service.InfoMsg                       (InfoMsg (..),
                                                        LogingTag (..),
                                                        MsgType (..))
import           Service.Transaction.Balance
-- import           Service.Transaction.Independent
import           Control.Concurrent.Chan.Unagi.Bounded
import           Data.List
import           Service.Transaction.Decode
import           Service.Transaction.Sprout
import           Service.Transaction.SproutCommon
import           Service.Transaction.Storage
import           Service.Types


bdLog :: InChan InfoMsg -> String -> IO ()
bdLog i msg = writeLog i [BDTag] Info msg


myTail ::  Common -> IO (Number, HashOfKeyBlock)
myTail c@(Common _ i) = do
    curNumber <- getKeyBlockNumber c
    (nNumber, hashMaybe) <- findChain c (fromJust curNumber) Main
    bdLog i $ "Current number of key block: " ++ show curNumber
    bdLog i $ "Get number of key block: " ++ show nNumber ++ "Hash: " ++ show hashMaybe
    mm <- findMicroblocksForMainChain c
    bdLog i $ "findMicroblocksForMainChain: " ++ show mm
    case hashMaybe of
        Nothing -> throw NoLastKeyBlock
        Just h  -> return (nNumber, h)

peekNPreviousKeyBlocks :: Common -> From -> To -> IO [(Number, HashOfKeyBlock)]
peekNPreviousKeyBlocks c from to = do
    vM <- forM [from..to] $ getM c
    return [(k, v) | (k, Just v) <- zip [from..to] vM]


getKeyBlockSproutData :: Common -> From -> To -> IO [(Number, HashOfKeyBlock, MacroblockBD)]
getKeyBlockSproutData c@(Common descr i) from to = do
  _ <- bdLog i <$> ("findMicroblocksForMainChain: " ++) <$> show <$> findMicroblocksForMainChain c
  kv <- peekNPreviousKeyBlocks c from to
  mb <- mapM (\(_,aHash) -> getKeyBlockByHash descr i (Hash aHash)) kv
  let allMaybe   = zipWith (\(aNumber, aHash) aMacroblock -> (aNumber, aHash, aMacroblock)) kv mb
      allJust    = filter (\(_,_,v) -> v /= Nothing) allMaybe
      allKeyData = map (\(n,h,m) -> (n, h, fromJust m)) allJust
  return allKeyData


pair3 :: (a, b, c) -> c
pair3 (_, _, c)  = c

pair2 :: (a, b, c) -> (b, c)
pair2 (_, b, c) = (b, c)

isValidKeyBlockSprout :: Common -> [(Number, HashOfKeyBlock, MacroblockBD)] -> IO Bool
isValidKeyBlockSprout c@(Common _ i) okv = do
    _ <- bdLog i <$> ("findMicroblocksForMainChain: " ++) <$> show <$> findMicroblocksForMainChain c
    _ <- bdLog i $ show $ intersperse "" $ map show okv
    let (numberAfterFoundation, _, macroblockAfterFoundation):_ = okv
    hashMainMaybe <- getM c (numberAfterFoundation - 1)
    let prevHash = _prevHKBlock (macroblockAfterFoundation :: MacroblockBD)
        isFoundationOk = hashMainMaybe == prevHash
        -- hash of Key Block is real hash
        kv = map pair2 okv
        fun h m = (h ==) $ getKeyBlockHash $ tKeyBlockToPoWType $ tMacroblock2KeyBlockInfo m
        hmm = map (\(h,m) -> (h, m, (fun h m) :: Bool)) kv
        isRealHash = map pair3 hmm
        allTrue = and $ isFoundationOk : isRealHash
    when (not allTrue) $ do  bdLog i $ concat $ map show isRealHash
    return allTrue


setKeyBlockSproutData :: Common -> [(HashOfKeyBlock,MacroblockBD)] -> IO ()
setKeyBlockSproutData c@(Common descr i) kv = do
  _ <- bdLog i <$> ("findMicroblocksForMainChain: " ++) <$> show <$> findMicroblocksForMainChain c
  bdLog i $ show $ kv
  -- mapM_ (\(h,m) -> updateMacroblockByKeyBlock descr i h (tMacroblock2KeyBlockInfo m) Sprout) kv
  mapM_ (\(h, m) -> updateMacroblockByMacroblock descr i h  m Sprout) kv
  mb <- getAllMacroblockByHash c $ map fst kv
  writeLog i [BDTag,KeyBlockTag] Info $ "After setting to db Macroblocks: " ++ (concat $ map show $ mb)

getRestSproutData :: Common -> HashOfMicroblock -> IO MicroBlockContent
getRestSproutData c@(Common descr i) hashOfMicroblock = do
  _ <- bdLog i <$> ("findMicroblocksForMainChain: " ++) <$> show <$> findMicroblocksForMainChain c

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
    _ <- bdLog i <$> ("findMicroblocksForMainChain: " ++) <$> show <$> findMicroblocksForMainChain c
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
    chain <- findWholeChainSince c aNumber Sprout
    mapM_ (\r -> deleteSprout c r Sprout) chain


deleteSprout :: Common -> (Number, HashOfKeyBlock) -> BranchOfChain -> IO () -- right after foundation
deleteSprout c@(Common descr i) (aNumber, hashOfKeyBlock) branch = do
  mm <- findMicroblocksForMainChain c
  bdLog i $ "findMicroblocksForMainChain: " ++ show mm
  macroblock <- getKeyBlockByHash descr i (Hash hashOfKeyBlock)
  case macroblock of
    Nothing -> writeLog i [BDTag] Error ("There is no KeyBlock "  ++ show hashOfKeyBlock)
    Just m -> do
      let hashesOfMicroBlocks = _mblocks (m :: MacroblockBD)
      microblocks <- mapM (\h -> getMicroBlockByHashDB descr (Hash h)) hashesOfMicroBlocks
      let hashesOfTransactions = concat $ map _transactionsHashes microblocks
      -- delete Transactions
      bdLog i $ "delete Transactions: " ++ show hashesOfTransactions
      mapM_ (funD (poolTransaction descr)) hashesOfTransactions
      -- delete MicroBlocks
      bdLog i $ "delete MicroBlocks: " ++ show hashesOfMicroBlocks
      mapM_ (funD (poolMicroblock descr)) hashesOfMicroBlocks
      -- delete KeyBlock
      bdLog i $ "delete KeyBlock: " ++ show hashOfKeyBlock
      funD (poolMacroblock descr) hashOfKeyBlock
      -- erase chain from Sprout table
      bdLog i $ "erase chain from Sprout table"
      (aMain,aSprout) <- getChain c aNumber
      let newChain = case branch of
            Main   -> (Nothing, aSprout)
            Sprout ->  (aMain, Nothing)
          sKey   = S.encode aNumber
          sValue = S.encode newChain
      bdLog i $ "new chain value: " ++ show aNumber ++ " " ++ show newChain
      funW (poolSprout descr) [(sKey, sValue)]


setSproutAsMain :: Common -> Number -> IO () -- right after foundation
setSproutAsMain c@(Common descr i) aNumber = do
    mm <- findMicroblocksForMainChain c
    bdLog i $ "findMicroblocksForMainChain: " ++ show mm
    -- find key blocks which belong to Main chain (right after foundation of main and sprout chain)
    mainChain            <- findWholeChainSince c aNumber Main
    mainChainKeyBlocks   <- getAllMacroblockByHash c $ map snd mainChain
    sproutChain          <- findWholeChainSince c aNumber Sprout
    sproutChainKeyBlocks <- getAllMacroblockByHash c $ map snd sproutChain
    -- recalculate ledger
    -- storno
    mapM_ (\(h,m) -> calculateLedger descr i True h m) mainChainKeyBlocks
    -- add closed sprout macroblocks to ledger
    mapM_ (\(h,m) -> calculateLedger descr i False h m) sproutChainKeyBlocks
    -- delete Main chain (right after foundation of main and sprout chain)
    mapM_ (\r -> deleteSprout c r Sprout) mainChain

    -- write Last Macroblock
    let lastClosedKeyBlockSprout = fst $ last sproutChainKeyBlocks
    funW (poolLast descr) [(lastClosedKeyBlock,lastClosedKeyBlockSprout)]

    -- set SproutChain as MainChain
    bdLog i $ "set SproutChain as MainChain " ++ show sproutChain
    mapM_ (\(n,h) -> setChainAndDeleteOther c n h Main) sproutChain
    let lastNumber = fst $ last sproutChain
    writeKeyBlockNumber c lastNumber


-- get all closed macroblocks for calculating ledger
getAllMacroblockByHash :: Common -> [HashOfKeyBlock] -> IO [(HashOfKeyBlock, MacroblockBD)]
getAllMacroblockByHash co hashesOfKeyBlock = do
  forM hashesOfKeyBlock $ findAllMacroblocks co
  -- return $ map fromJust $ filter (/= Nothing ) macroblocks
  where
        findAllMacroblocks :: Common -> HashOfKeyBlock -> IO (HashOfKeyBlock, MacroblockBD)
        findAllMacroblocks (Common descr i) h = do
          macroblockMaybe <- getKeyBlockByHash descr i (Hash h)
          case macroblockMaybe of
            Nothing -> do
              writeLog i [BDTag] Error $ "NoKeyBlock " ++ show h
              throw $ NoKeyBlock $ show h
            Just (j :: MacroblockBD) -> do
              when (null $ _mblocks (j :: MacroblockBD)) $
                  writeLog i [BDTag] Warning $ "mblocks is empty for hash: " ++ show h
              return $ (h, j)
              -- isMacroblockClosed <- checkMacroblockIsClosed j i
              -- writeLog i [BDTag] Info $ "Macroblock with hash " ++ show h ++ "is closed " ++ show isMacroblockClosed
              -- if isMacroblockClosed
              -- then return $ Just (h, j)
              -- else return Nothing


kBlock1 :: HashOfKeyBlock
kBlock1 = "AAABxBQmBTqnwHo9fh2Q7yXmkjbTdLHbIVuetMGhzQk="


findMicroblocksForMainChain  :: Common -> IO [HashOfMicroblock]
findMicroblocksForMainChain  c = do
  mainChainKHashes <- map snd <$> findWholeChainSince c 0 Main
  macroblock <- map snd <$> getAllMacroblockByHash c mainChainKHashes
  let hashesOfMicroblocks = concat $ map (\m -> _mblocks (m :: MacroblockBD)) macroblock
  return hashesOfMicroblocks
