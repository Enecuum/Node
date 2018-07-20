{-# LANGUAGE DuplicateRecordFields #-}

module Service.Transaction.LedgerSync where


import           Control.Exception
import           Control.Monad.Loops
import           Data.Maybe
import qualified Data.Serialize                   as S (encode)
import           Node.Data.GlobalLoging
import           Service.InfoMsg                  (LogingTag (..), MsgType (..))
import           Service.Transaction.Balance
import           Service.Transaction.Sprout
import           Service.Transaction.SproutCommon
import           Service.Transaction.Storage
import           Service.Types

myTail ::  Common -> IO (Number, HashOfKeyBlock)
myTail (Common descr i _) = do
  kv <- getLastKeyBlock descr i
  case kv of
    Nothing -> throw NoClosedKeyBlockInDB
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
getKeyBlockSproutData c@(Common descr i _) from to = do
  kv <- peekNPreviousKeyBlocks c from to
  mb <- mapM (\(_,aHash) -> getKeyBlockByHash descr (Hash aHash) i) kv
  let allMaybe = zipWith (\(number, aHash) aMacroblock -> (number, aHash, aMacroblock)) kv mb
      allJust = filter (\(_,_,v) -> v /= Nothing) allMaybe
      allKeyData = map (\(n,h,m) -> (n, h, fromJust m)) allJust
  return allKeyData


isValidKeyBlockSprout :: Common -> (HashOfKeyBlock, MacroblockBD) -> IO Bool
isValidKeyBlockSprout = undefined -- Fix verify hash of KeyBlock
-- -- tMacroblock2KeyBlockInfo


setKeyBlockSproutData :: Common -> [(HashOfKeyBlock,MacroblockBD)] -> IO ()
setKeyBlockSproutData c@(Common descr _ _) kv = do
  -- write MacroblockBD without Microblocks hashes
  let clearMicroblocks = map (\(k,v ) -> (k,fun v) ) kv
        where fun v = (v {_mblocks = []}) :: MacroblockBD
  let kvDB = map (\(k,v) -> (k, S.encode v)) clearMicroblocks
  funW (poolMacroblock descr) kvDB

  -- read from and write to Sprout Table
  let kvN = map (\(h,m) -> (h, _number (m :: MacroblockBD))) kv
  mapM_ (\(hashOfKeyBlock, number) -> setS c number hashOfKeyBlock) kvN


getRestSproutData :: Common -> HashOfMicroblock -> IO MicroBlockContent
getRestSproutData (Common descr _ _) hashOfMicroblock = do
  microblock <- getMicroBlockByHashDB descr (Hash hashOfMicroblock)
  case microblock of Nothing -> throw NoSuchMicroBlockDB
                     Just m -> do
                       tx <- getTransactionsByMicroblockHash descr (Hash hashOfMicroblock)
                       case tx of Nothing -> throw NoSuchTransactionDB
                                  Just t  -> return $ MicroBlockContent m t


isValidRestOfSprout :: Common -> MicroBlockContent -> IO Bool
isValidRestOfSprout _ _ = do -- Fix verify transaction signature
  return True


setRestSproutData :: Common -> (Number, HashOfKeyBlock, MicroBlockContent) -> IO ()
setRestSproutData c@(Common descr i _) (number, hashOfKeyBlock, (MicroBlockContent mb txInfo )) = do
  -- write MicroBlockContent MicroblockBD [TransactionInfo]
  let tx = map (\t -> _tx (t :: TransactionInfo)) txInfo
  writeTransactionDB descr i tx (rHash mb)
  writeMicroblockDB descr i mb

  -- add hashes of microblocks to Macroblock table
  addMicroblockHashesToMacroBlock descr i hashOfKeyBlock [rHash mb]
  -- write number and hashOfKeyBlock to Sprout table
  setS c number hashOfKeyBlock


deleteSproutData      :: Common -> (Number, HashOfKeyBlock) -> IO () -- right after foundation
deleteSproutData c@(Common descr i _) (number, hashOfKeyBlock) = do
  deleteSprout c (number, hashOfKeyBlock) Sprout


-- deleteWholeChainSince ::  Common -> (Number, HashOfKeyBlock) -> IO ()
findWholeChainSince ::  Common -> (Number, HashOfKeyBlock) -> BranchOfChain -> IO [(Number, HashOfKeyBlock)]
findWholeChainSince c (number, hashofkeyblock) branch = do --undefined
  let fun = findChain c (number, hashofkeyblock) branch
      second = \a -> isNothing $ snd a
  -- concat <$>
  whileM (second <$> fun) $ findWholeChainSince c (number, hashofkeyblock) branch
  return undefined


-- func :: <function type>
-- func <arguments> =
--     if condition
--         then <recursive call>
--         else computedValue

deleteSprout :: Common -> (Number, HashOfKeyBlock) -> BranchOfChain -> IO () -- right after foundation
deleteSprout (Common descr i _) (number, hashOfKeyBlock) branch = do
  macroblock <- getKeyBlockByHash descr (Hash hashOfKeyBlock) i
  case macroblock of
    Nothing -> writeLog i [BDTag] Error ("There is no KeyBlock "  ++ show hashOfKeyBlock)
    Just m -> do
      let hashesOfMicroBlocks = _mblocks (m :: MacroblockBD)
      microblocksMaybe <- mapM (\h -> getMicroBlockByHashDB descr (Hash h)) hashesOfMicroBlocks
      let microblocks = map fromJust microblocksJust
            where microblocksJust = filter (/= Nothing) microblocksMaybe
          hashesOfTransactions = concat $ map _transactionsHashes microblocks
      -- delete Transactions
      mapM_ (funD (poolTransaction descr)) hashesOfTransactions
      -- delete MicroBlocks
      mapM_ (funD (poolMicroblock descr)) hashesOfMicroBlocks
      -- delete KeyBlock
      funD (poolMacroblock descr) hashOfKeyBlock
      -- delete chain
      -- chain <- getChain number
      -- newChain = case branch of Main -> (Noting, snd chain)
      --                           Sprout -> ()
      -- funW (poolSprout descr) [(S.encode number), (S.encode newChain)


setSproutAsMain       :: Common -> (Number, HashOfKeyBlock) -> IO () -- right after foundation
setSproutAsMain = undefined
