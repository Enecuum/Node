{-# LANGUAGE DuplicateRecordFields #-}

module Service.Transaction.LedgerSync where

import           Control.Concurrent.Chan.Unagi.Bounded
import qualified Data.ByteString.Internal              as BSI
import           Data.Maybe
import           Service.InfoMsg                       (InfoMsg (..))
import           Service.Transaction.Storage
import           Service.Types


myTail :: DBPoolDescriptor -> InChan InfoMsg -> IO Integer
myTail desc aInfoChan = do
  (_,mb) <- getLastKeyBlock desc aInfoChan
  let mbN = fromJust mb
  let n = _number (mbN :: MacroblockBD)
  return n

type HashOfKeyBlock = BSI.ByteString
peekNPreviousClosedKeyBlocks :: DBPoolDescriptor -> InChan InfoMsg -> Int -> HashOfKeyBlock -> IO [HashOfKeyBlock]
peekNPreviousClosedKeyBlocks = undefined

getFromDBMacroblocks :: DBPoolDescriptor -> InChan InfoMsg -> [HashOfKeyBlock] -> IO [MacroblockBD]
getFromDBMacroblocks = undefined

-- canAppendChainToKeyBlock ::
data SproutInfo = SproutInfo [MacroblockBD] [MicroblockBD] [TransactionInfo]

createSprout :: DBPoolDescriptor -> InChan InfoMsg -> SproutInfo -> IO ()
createSprout = undefined

type From = Int
type To = Int
getSprout :: DBPoolDescriptor -> InChan InfoMsg -> From -> To -> IO SproutInfo
getSprout = undefined
