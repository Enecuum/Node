{-# LANGUAGE OverloadedStrings, LambdaCase, ScopedTypeVariables #-}

module Main where

import              Control.Monad
import              Control.Concurrent
import              System.Environment (getArgs, getEnv)

import              Node.Node.Mining
import              Node.Node.Types
import              Service.Timer
import              Service.Config
import              Node.Lib
import              Service.Metrics
import              PoA
import              CLI.CLI (control)
import              Control.Exception (try)
import              Prelude hiding (concat)
import              Data.Ini
import              Data.Text
import              Network.Socket (PortNumber)
import              Control.Exception (SomeException())

main :: IO ()
main = do
    (readIniFile "configs/config.ini") >>= \case
       Left e    -> error e
       Right ini -> do
        aExitCh <- newChan
        aAnswerCh  <- newChan

        metric $ increment "cl.node.count"
        
        void $ startNode ini
            aExitCh aAnswerCh managerMining $ \ch aChan aMyNodeId -> do
                -- periodically check current state compare to the whole network state
                metronomeS 400000 (writeChan ch connectivityQuery)
                metronomeS 1000000 (writeChan ch deleteOldestMsg)
                metronomeS 10000000 (writeChan ch deleteDeadSouls)
                metronomeS 3000000 $ writeChan ch deleteOldestVacantPositions

                poa_in  <- getConfigValue ini "poa" "InpPort"
                poa_out <- getConfigValue ini "poa" "OutPort"
                void $ forkIO $ servePoA poa_in  aMyNodeId ch aChan poa_out

                rpc_port <- getConfigValue ini "rpc" "Port"
                void $ forkIO $ control rpc_port ch
        void $ readChan aExitCh

