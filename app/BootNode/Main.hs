{-# LANGUAGE OverloadedStrings, LambdaCase, ScopedTypeVariables #-}

-- Boot node's binaries
module Main where

import              Control.Monad
import              Control.Exception(SomeException, try)
import              Control.Concurrent
import              Service.Timer
import              Service.InfoMsg
import              System.Environment
import              Node.Node.Types

import              Network.Socket (inet_addr)
import qualified    Data.ByteString.Lazy as L

import              Boot.Boot
import              Boot.Types
import              Node.Lib
import              Data.Aeson

main :: IO ()
main =  do
      enc <- L.readFile "configs/config.json"
      case (decode enc) :: Maybe BuildConfig of
          Nothing   -> error "Please, specify config file correctly"
          Just conf -> do

            exitCh <- newChan
            answerCh <- newChan
            infoCh <- newChan

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


            void $ startNode conf
              exitCh answerCh infoCh managerBootNode $ \ch _ _ -> do
                  metronomeS 100000 (writeChan ch checkBroadcastNodes)

                  void $ forkIO $ serveInfoMsg stat_h stat_p infoCh
            void $ readChan exitCh
