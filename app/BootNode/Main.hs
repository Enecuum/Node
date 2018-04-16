{-# LANGUAGE OverloadedStrings, LambdaCase #-}

-- Boot node's binaries
module Main where

import              Control.Monad
import              Control.Concurrent
import              Service.Timer
import              Node.Node.Types

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
            metricCh <- newChan

            void $ startNode conf
              exitCh answerCh metricCh managerBootNode $ \ch _ _ -> do
                  metronomeS 100000 (writeChan ch checkBroadcastNodes)
            void $ readChan exitCh
