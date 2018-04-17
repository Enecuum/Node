{-# LANGUAGE OverloadedStrings, LambdaCase, ScopedTypeVariables #-}

module Main where

import              Control.Monad
import              Control.Concurrent
import              System.Environment (getEnv)

import              Node.Node.Mining
import              Node.Node.Types
import              Service.Timer
import              Node.Lib
import              Service.InfoMsg
import              PoA
import              CLI.CLI (serveRpc)
import              Control.Exception (try)
import              Prelude hiding (concat)
import              Network.Socket (inet_addr)
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
            aInfoCh   <- newChan

            void $ startNode conf
                aExitCh aAnswerCh aInfoCh managerMining $ \ch aChan aMyNodeId -> do
                    -- periodically check current state compare to the whole network state
                    metronomeS 400000 (writeChan ch connectivityQuery)
                    metronomeS 1000000 (writeChan ch deleteOldestMsg)
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
                            Right item              -> inet_addr item
                            Left (_::SomeException) -> case statsdBuildConfig conf of
                                 Nothing   -> error "Please, specify statsdConfig"
                                 Just stat -> inet_addr $ statsdHost stat

                    stat_p  <- try (getEnv "statsdPort") >>= \case
                            Right item              -> return $ read item
                            Left (_::SomeException) -> case statsdBuildConfig conf of
                                 Nothing   -> error "Please, specify statsdConfig"
                                 Just stat -> return $ statsdPort stat

                    void $ forkIO $ serveInfoMsg stat_h stat_p aInfoCh (toInteger aMyNodeId)

                    void $ forkIO $ servePoA poa_in poa_out aMyNodeId ch aChan aInfoCh
                    void $ forkIO $ serveRpc rpc_p ch aInfoCh

                    writeChan aInfoCh $ Metric $ increment "cl.node.count"

            void $ readChan aExitCh
