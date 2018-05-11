{-# LANGUAGE OverloadedStrings, LambdaCase, ScopedTypeVariables #-}

-- Boot node's binaries
module Main where

import              Control.Monad
import              Control.Exception(SomeException, try)
import              Control.Concurrent
import              Service.Timer
import              Service.InfoMsg
import              Service.Network.Base (ConnectInfo(..))
import              System.Environment
import              Node.Node.Types
import              PoA.PoAServer

import              Network.Socket()
import qualified    Data.ByteString.Lazy as L

import              Boot.Boot
import              Boot.Types
import              Node.Lib
import              Data.Aeson

main :: IO ()
main =  do
      enc <- L.readFile "configs/config.json"
      case decode enc :: Maybe BuildConfig of
          Nothing   -> error "Please, specify config file correctly"
          Just conf -> do

            exitCh <- newChan
            answerCh <- newChan
            aInfoChan <- newChan

            poa_in  <- try (getEnv "poaInPort") >>= \case
                    Right item              -> return $ read item
                    Left (_::SomeException) -> case simpleNodeBuildConfig conf of
                         Nothing   -> error "Please, specify SimpleNodeConfig"
                         Just snbc -> return $ poaInPort snbc

            stat_h  <- try (getEnv "statsdHost") >>= \case
                    Right item              -> return item
                    Left (_::SomeException) -> return $ host $ statsdBuildConfig conf

            stat_p  <- try (getEnv "statsdPort") >>= \case
                    Right item              -> return $ read item
                    Left (_::SomeException) -> return $ port $ statsdBuildConfig conf

            logs_h  <- try (getEnv "logHost") >>= \case
                    Right item              -> return item
                    Left (_::SomeException) -> return $ host $ logsBuildConfig conf

            logs_p  <- try (getEnv "logPort") >>= \case
                    Right item              -> return $ read item
                    Left (_::SomeException) -> return $ port $ logsBuildConfig conf



            void $ startNode conf
              exitCh answerCh aInfoChan managerBootNode $ \ch _ aNodeId aFileChan -> do
                  log_id  <- try (getEnv "logId") >>= \case
                    Right item              -> return item
                    Left (_::SomeException) -> return $ show aNodeId
                  metronomeS 100000 (writeChan ch checkBroadcastNodes)
                  void $ forkIO $ serverPoABootNode poa_in aInfoChan aFileChan
                  void $ forkIO $ serveInfoMsg (ConnectInfo stat_h stat_p) (ConnectInfo logs_h logs_p) aInfoChan log_id
            void $ readChan exitCh
