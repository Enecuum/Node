-- Boot node's binaries
module Main where

import              Control.Monad
import              Control.Concurrent
import              Service.Timer
import              Node.Node.Types

import              Boot.Boot
import              Boot.Types
import              Node.Lib

main :: IO ()
main = do
    exitCh    <- newChan
    answerCh  <- newChan
    void $ startNode "./data/bootInitData.bin"
        exitCh answerCh managerBootNode $ \ch _ _ -> do
            metronomeS 100000 (writeChan ch checkBroadcastNodes)
            metronomeS 10000000 (writeChan ch deleteDeadSouls)
    void $ readChan exitCh
