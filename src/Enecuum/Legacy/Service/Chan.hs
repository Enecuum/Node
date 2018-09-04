module Enecuum.Legacy.Service.Chan where

import           Control.Concurrent
import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Monad
import           Universum

-- safe way to write in chan
writeInChan :: InChan t -> t -> IO ()
writeInChan aChan aMsg = do
    aOk <- tryWriteChan aChan aMsg
    threadDelay 10000
    unless aOk $ writeInChan aChan aMsg
