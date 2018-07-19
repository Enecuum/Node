{-# LANGUAGE DuplicateRecordFields #-}

module Service.Transaction.LedgerSync where

import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Exception
import qualified Data.ByteString.Internal              as BSI
import qualified Data.HashTable.IO                     as H
import           Service.InfoMsg                       (InfoMsg (..))
import           Service.Transaction.Balance
import           Service.Transaction.Storage
import           Service.Types

type HashOfMicroblock = BSI.ByteString
type HashOfKeyBlock = BSI.ByteString
data MicroBlockContent = MicroBlockContent [MicroblockBD] [TransactionInfo]
type Number = Integer
type From = Number
type To = Number


type SproutTable = H.BasicHashTable Number [Maybe HashOfKeyBlock]
data CommonData = CommonData {
  db       :: DBPoolDescriptor,
  infoChan :: InChan InfoMsg,
  sprout   :: SproutTable}


sproutTable :: IO SproutTable
sproutTable = sproutT
  where v1 = [Just (read "1" :: HashOfKeyBlock)]
        v2 = [Just (read "2" :: HashOfKeyBlock)]
        v3 = [Just (read "3" :: HashOfKeyBlock)]
        kv = [(1, v1), (2, v2), (3, v3)]
        sproutT = H.fromList kv

-- H.lookup ht $ key
-- H.insert ht key value

myTail ::  DBPoolDescriptor -> InChan InfoMsg -> IO (HashOfKeyBlock, Number)
myTail descr i = do
  kv <- getLastKeyBlock descr i
  case kv of
    Nothing -> throw NoClosedKeyBlockInDB
    Just (hashOfKeyBlock, mb)  -> do
      let n =  _number (mb :: MacroblockBD)
      return (hashOfKeyBlock, n)


-- peekNPreviousClosedKeyBlocks :: CommonData -> Int -> HashOfKeyBlock -> IO [(HashOfKeyBlock, Number)]
peekNPreviousKeyBlocks :: CommonData -> From -> To -> IO [(HashOfKeyBlock, Number)]
peekNPreviousKeyBlocks c from to = do --undefined
  let numbers = [from .. to]
  st <- sproutTable
  let kv = map (\n -> (H.lookup st n, n)) numbers
        -- where hashOfKeyBlock n = fromJust (H.lookup st n)
  return undefined

getKeyBlockSproutData :: CommonData -> From -> To -> IO [(HashOfKeyBlock,MacroblockBD)]
getKeyBlockSproutData = undefined


isValidKeyBlockSprout :: CommonData -> [(HashOfKeyBlock,MacroblockBD)] -> IO Bool
isValidKeyBlockSprout = undefined


setKeyBlockSproutData :: CommonData -> [(HashOfKeyBlock,MacroblockBD)] -> IO ()
setKeyBlockSproutData = undefined


getRestSproutData :: CommonData -> HashOfMicroblock -> IO [MicroBlockContent]
getRestSproutData = undefined


isValidRestOfSprout :: CommonData -> [MicroBlockContent] -> IO Bool
isValidRestOfSprout = undefined


setRestSproutData :: CommonData -> [MicroBlockContent] -> IO ()
setRestSproutData = undefined


sproutFullyTransfered :: CommonData -> IO ()
sproutFullyTransfered = undefined



-------------------------
-- tMacroblock2KeyBlockInfo
