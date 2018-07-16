{-# LANGUAGE DuplicateRecordFields #-}

module Service.Transaction.LedgerSync where

import           Control.Concurrent.Chan.Unagi.Bounded
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
