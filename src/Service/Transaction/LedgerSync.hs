{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Service.Transaction.LedgerSync where

import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Exception
import           Control.Monad                         (forM, unless, when)
import           Data.List
import           Data.Maybe
import qualified Data.Serialize                        as S (encode)
import qualified "rocksdb-haskell" Database.RocksDB    as Rocks
import           Node.Data.GlobalLoging
import           Service.InfoMsg                       (LogingTag (..),
                                                        MsgType (..))
import           Service.Sync.SyncJson
import           Service.Sync.SyncTypes
import           Service.Transaction.Balance
import           Service.Transaction.Decode
import           Service.Transaction.Sprout
import           Service.Transaction.Storage
import           Service.Transaction.Transformation
import           Service.Types


myTail ::  Common -> IO (Number, HashOfKeyBlock)
myTail c@(Common _ i) = do
    curNumber <- getKeyBlockNumber c
    (nNumber, hashMaybe) <- findChain c (fromJust curNumber) Main
    bdLog i $ "Current number of key block: " ++ show curNumber
    bdLog i $ "Get number of key block: " ++ show nNumber ++ "Hash: " ++ show hashMaybe
    case hashMaybe of
        Nothing -> throw NoLastKeyBlock
        Just h  -> return (nNumber, h)


peekNPreviousKeyBlocks :: Common -> From -> To -> IO [(Number, HashOfKeyBlock)]
peekNPreviousKeyBlocks c from to = do
    vM <- forM [from..to] $ getM c
    return [(k, v) | (k, Just v) <- zip [from..to] vM]


getKeyBlockSproutData :: Common -> From -> To -> IO [(Number, HashOfKeyBlock, KeyBlockContent)]
getKeyBlockSproutData c from to = do
  kv <- peekNPreviousKeyBlocks c from to
  mapM fun kv
  where
    fun :: (Number, HashOfKeyBlock) -> IO (Number, HashOfKeyBlock, KeyBlockContent)
    fun (aNumber, hashofkeyblock) = do
          mbBD <- getKeyBlockByHash c $ Hash hashofkeyblock
          case mbBD of
            Nothing -> throw $ NoKeyBlock $ "with hash " ++ show hashofkeyblock
            Just j  -> do
              let hashesOfMicroblocks = _mblocks (j :: MacroblockBD)
                  keyBlock = (tKeyBlockToPoWType . tMacroblock2KeyBlockInfo) j
                  keyBlockContent = KeyBlockContent keyBlock hashesOfMicroblocks
              return (aNumber, hashofkeyblock, keyBlockContent)


pair3 :: (a, b, c) -> c
pair3 (_, _, c)  = c

pair2 :: (a, b, c) -> (b, c)
pair2 (_, b, c) = (b, c)


isValidKeyBlockSprout :: Common -> [(Number, KeyBlockContent)] -> IO Bool
isValidKeyBlockSprout c@(Common _ i) okv = do
    bdLog i $ show $ intersperse "" $ map show okv
    let (numberAfterFoundation, (KeyBlockContent keyBlockAfterFoundation _)) = head okv
    hashMainMaybe <- getM c (numberAfterFoundation - 1)
    let prevHash = case _prev_hash (keyBlockAfterFoundation :: KeyBlockInfoPoW) of
          "" -> Nothing
          s  -> Just s
        isFoundationOk = hashMainMaybe == prevHash

    -- check that all chain is linked
    let kBlocks = map (  (\(KeyBlockContent k _) -> k) . snd) okv
        fun :: KeyBlockInfoPoW -> KeyBlockInfoPoW -> Bool
        fun p n = getKeyBlockHash p == _prev_hash (n :: KeyBlockInfoPoW)
        isGood index = fun (kBlocks !! index) (kBlocks !! (index+1))
        isChainReallyLinked = map (\(_,index) -> isGood index) $ zip kBlocks [0..]
        isChainLinked = map (const True) kBlocks
        isOk = isFoundationOk : isChainLinked
        allTrue = and isOk

    unless allTrue $ bdLog i $ concatMap show (isFoundationOk : isChainReallyLinked )
    return allTrue


setKeyBlockSproutData :: Common -> (InChan SyncEvent, OutChan SyncEvent) -> [KeyBlockInfoPoW] -> IO ()
setKeyBlockSproutData c aSyncChan kBlocks = do
  -- bdLog i $ show kv
  let fun1 k = addKeyBlockToDB2 c k aSyncChan
      fun2 k = setChain c (_number (k :: KeyBlockInfoPoW)) (getKeyBlockHash k) Sprout
  mapM_ fun1 kBlocks
  mapM_ fun2 kBlocks



getRestSproutData :: Common -> HashOfMicroblock -> IO MicroBlockContent
getRestSproutData c hashOfMicroblock = do
  microblockBD <- getMicroBlockByHashDB c (Hash hashOfMicroblock)
  microblock <- tMicroblockBD2Microblock c microblockBD
  return $ MicroBlockContent microblock


isValidRestOfSprout :: Common -> MicroBlockContent -> IO Bool
isValidRestOfSprout _ _ = return True -- Fix verify transaction signature


setRestSproutData :: Common -> (Number, HashOfKeyBlock, MicroBlockContent) -> IO ()
setRestSproutData c (aNumber, hashOfKeyBlock, MicroBlockContent mb) = do
    findMicroblocksForMainChain c
    addMicroblockToDB c mb Sprout
    -- write number and hashOfKeyBlock to Sprout table
    setChain c aNumber hashOfKeyBlock Sprout


deleteSproutData      :: Common -> Number -> IO () -- right after foundation
deleteSproutData c aNumber = do
    chain <- findWholeChainSince c aNumber Sprout
    mapM_ (\r -> deleteSprout c r Sprout) chain


deleteSprout :: Common -> (Number, HashOfKeyBlock) -> BranchOfChain -> IO () -- right after foundation
deleteSprout c@(Common descr i) (aNumber, hashOfKeyBlock) branch = do
  findMicroblocksForMainChain c
  macroblock <- getKeyBlockByHash c (Hash hashOfKeyBlock)
  case macroblock of
    Nothing -> writeLog i [BDTag] Error ("There is no KeyBlock "  ++ show hashOfKeyBlock)
    Just m -> do
      let hashesOfMicroBlocks = _mblocks (m :: MacroblockBD)
      microblocks <- mapM (getMicroBlockByHashDB c . Hash) hashesOfMicroBlocks
      let hashesOfTransactions = concatMap _transactionsHashes microblocks
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
      bdLog i "erase chain from Sprout table"
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
    findMicroblocksForMainChain c
    -- find key blocks which belong to Main chain (right after foundation of main and sprout chain)
    mainChain            <- findWholeChainSince c aNumber Main
    mainChainKeyBlocks   <- getAllMacroblockByHash c $ map snd mainChain
    sproutChain          <- findWholeChainSince c aNumber Sprout
    sproutChainKeyBlocks <- getAllMacroblockByHash c $ map snd sproutChain
    -- recalculate ledger
    -- storno
    mapM_ (uncurry (calculateLedger c True)) mainChainKeyBlocks
    -- add closed sprout macroblocks to ledger
    mapM_ (uncurry (calculateLedger c False)) sproutChainKeyBlocks
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
getAllMacroblockByHash co hashesOfKeyBlock = forM hashesOfKeyBlock $ findAllMacroblocks co
  where
        findAllMacroblocks :: Common -> HashOfKeyBlock -> IO (HashOfKeyBlock, MacroblockBD)
        findAllMacroblocks (Common _ i) h = do
          macroblockMaybe <- getKeyBlockByHash co (Hash h)
          case macroblockMaybe of
            Nothing -> do
              writeLog i [BDTag] Error $ "NoKeyBlock " ++ show h
              throw $ NoKeyBlock $ show h
            Just (j :: MacroblockBD) -> do
              when (null $ _mblocks (j :: MacroblockBD)) $
                  writeLog i [BDTag] Warning $ "mblocks is empty for hash: " ++ show h
              return (h, j)


findMicroblocksForMainChain :: Common -> IO ()
findMicroblocksForMainChain c@(Common _ i) = (bdLog i . ("findMicroblocksForMainChain: " ++) . show) =<< findMicroblocksForMainChain c


findMicroblocksForMainChainHelp  :: Common -> IO [HashOfMicroblock]
findMicroblocksForMainChainHelp  c = do
  mainChainKHashes <- map snd <$> findWholeChainSince c 0 Main
  macroblock <- map snd <$> getAllMacroblockByHash c mainChainKHashes
  let hashesOfMicroblocks = concatMap (\m -> _mblocks (m :: MacroblockBD)) macroblock
  return hashesOfMicroblocks


cleanDB :: Common -> IO ()
cleanDB (Common _ i) = do 
  filenames <- allDB
  mapM_ (\f -> Rocks.destroy f Rocks.defaultOptions) filenames
  bdLog i $ "Delete all tables"
