{-# LANGUAGE OverloadedStrings, LambdaCase, ScopedTypeVariables #-}

-- Boot node's binaries
module Main where

import              Control.Monad
import              Control.Exception(SomeException, try)
import qualified    Control.Concurrent as C
import              Control.Concurrent.Chan.Unagi.Bounded
import              Service.Timer
import              Service.InfoMsg
import              Service.Network.Base (ConnectInfo(..))
import              System.Environment
import              Node.Node.Types
import              PoA.PoAServer
import              Node.FileDB.FileServer

import              Network.Socket()
import qualified    Data.ByteString.Lazy as L


import              Node.Lib
import              Data.Aeson
import              Service.Transaction.Common (connectOrRecoveryConnect)


main :: IO ()
main =  do
      putStrLn  "Dev 25/06/2018 17:00"
      enc <- L.readFile "configs/config.json"
      case decode enc :: Maybe BuildConfig of
          Nothing   -> error "Please, specify config file correctly"
          Just conf -> do

            exitCh <- C.newChan
            answerCh <- C.newChan
            (aInfoChanIn, aInfoChanOut) <- newChan (2^5)
            descrDB   <- connectOrRecoveryConnect
            poa_p   <- try (getEnv "poaPort") >>= \case
                    Right item              -> return $ read item
                    Left (_::SomeException) -> return $ poaPort conf

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

            log_id  <- try (getEnv "log_id") >>= \case
                Right item              -> return item
                Left (_::SomeException) -> return "0"
            (aFileChan, aOutFileRequestChan) <- newChan (2^4)
            void $ C.forkIO $ startFileServer aOutFileRequestChan
            void $ C.forkIO $ serverPoABootNode poa_p aInfoChanIn aFileChan
            void $ C.forkIO $ serveInfoMsg (ConnectInfo stat_h stat_p) (ConnectInfo logs_h logs_p) aInfoChanOut log_id

            void $ C.readChan exitCh
