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

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as L

main :: IO ()
main =  do
        enc <- L.readFile "configs/config.json"
        case (decode enc) :: Maybe BuildConfig of
          Nothing   -> error "Please, specify config file correctly"
          Just conf -> do
     
            aExitCh <- newChan
            aAnswerCh <- newChan

            metric $ increment "cl.node.count"

            void $ startNode conf
                aExitCh aAnswerCh managerMining $ \ch aChan aMyNodeId -> do
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

                    void $ forkIO $ servePoA poa_in  aMyNodeId ch aChan poa_out
                    void $ forkIO $ serveRpc rpc_p ch

            void $ readChan aExitCh

