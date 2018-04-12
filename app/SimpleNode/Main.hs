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
import              CLI.CLI (serveRpc)
import              Control.Exception (try)
import              Prelude hiding (concat)
import              Data.Ini
import              Data.Text
import              Network.Socket (PortNumber)
import              Control.Exception (SomeException())

import              Data.Aeson
import qualified    Data.ByteString.Lazy as L

main :: IO ()
main =  do
        enc <- L.readFile "configs/config.json"
        case (decode enc) :: Maybe BuildConfig of
          Nothing   -> error "Please, specify config file correctly"
          Just conf -> do
     
            aExitCh   <- newChan
            aAnswerCh <- newChan
            aMetricCh <- newChan

            void $ startNode conf
                aExitCh aAnswerCh aMetricCh managerMining $ \ch aChan aMyNodeId -> do
                    -- periodically check current state compare to the whole network state
                    metronomeS 400000 (writeChan ch connectivityQuery)
                    metronomeS 1000000 (writeChan ch deleteOldestMsg)
                    metronomeS 10000000 (writeChan ch deleteDeadSouls)
                    metronomeS 3000000 $ writeChan ch deleteOldestVacantPositions
  
                    poa_in  <- try (getEnv "poaInPort") >>= \case
                            Right item              -> return $ read item
                            Left (_::SomeException) -> case simpleNodeBuildConfig conf of
                                 Nothing   -> error "Please, specify SimpleNodeConfig"
                                 Just snbc -> return $ poaInPort snbc

                    poa_out <- try (getEnv "poaOutPort") >>= \case
                            Right item              -> return $ read item
                            Left (_::SomeException) -> case simpleNodeBuildConfig conf of
                                 Nothing   -> error "Please, specify SimpleNodeConfig"
                                 Just snbc -> return $ poaOutPort snbc

                    rpc_p   <- try (getEnv "rpcPort") >>= \case
                            Right item              -> return $ read item
                            Left (_::SomeException) -> case simpleNodeBuildConfig conf of
                                 Nothing   -> error "Please, specify SimpleNodeConfig"
                                 Just snbc -> return $ rpcPort snbc

                    stat_h  <- try (getEnv "statsdHost") >>= \case
                            Right item              -> return $ read item
                            Left (_::SomeException) -> case statsdBuildConfig conf of
                                 Nothing   -> error "Please, specify statsdConfig"
                                 Just stat -> return $ read $ statsdHost stat

                    stat_p  <- try (getEnv "statsdPort") >>= \case
                            Right item              -> return $ read item
                            Left (_::SomeException) -> case statsdBuildConfig conf of
                                 Nothing   -> error "Please, specify statsdConfig"
                                 Just stat -> return $ statsdPort stat

                    void $ forkIO $ serveMetrics stat_h stat_p aMetricCh

                    void $ forkIO $ servePoA poa_in poa_out aMyNodeId ch aChan aMetricCh
                    void $ forkIO $ serveRpc rpc_p ch aMetricCh

                    writeChan aMetricCh $ increment "cl.node.count"

            void $ readChan aExitCh

