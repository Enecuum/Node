module Main where

import              Control.Monad
import              Control.Concurrent
import              System.Environment (getArgs)

import              Node.Node.Mining
import              Node.Node.Types
import              Service.Config
import              Service.Timer
import              Node.Lib
import              Service.Metrics
import              PoA
import              CLI.CLI (control)


main :: IO ()
main = do
    args <- getArgs
    maybeConf <- findConfigFile args
    case maybeConf of
      Nothing     -> return ()
      Just config -> do
        aExitChan <- newChan
        aAnswerChan  <- newChan
        metric $ increment "cl.node.count"
        void $ startNode "./data/miningInitData.bin"
            aExitChan aAnswerChan managerMining $ \ch aChan aMyNodeId -> do
                -- периодически проверяем, в каком состоянии относительно сети
                -- мы находимся
                metronomeS 400000 (writeChan ch connectivityQuery)
                metronomeS 1000000 (writeChan ch deleteOldestMsg)
                metronomeS 10000000 (writeChan ch deleteDeadSouls)
                metronomeS 3000000 $ writeChan ch deleteOldestVacantPositions

                Just poa_in  <- getVar config "SimpleNode" "poa_in"
                Just poa_out <- getVar config "SimpleNode" "poa_out"
                void $ forkIO $ servePoA poa_in  aMyNodeId ch aChan poa_out

                Just rpc_port <- getVar config "SimpleNode" "rpc"
                void $ forkIO $ control rpc_port ch
        void $ readChan aExitChan
