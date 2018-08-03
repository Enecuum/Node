{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Boot node's binaries
module Main where

import qualified Control.Concurrent                    as C
import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Exception                     (SomeException, try)
import           Control.Monad

import           BootNodeServer
import           Node.DataActor
import           Node.Node.Types
import           Service.InfoMsg
import           Service.Network.Base                  (ConnectInfo (..))
import           System.Environment

import           Data.Aeson
import qualified Data.ByteString.Lazy                  as L
import           Network.Socket                        ()


main :: IO ()
main =  do
      enc <- L.readFile "configs/config.json"
      case decode enc :: Maybe BuildConfig of
          Nothing   -> error "Please, specify config file correctly"
          Just conf -> do

            --answerCh <- C.newChan
            (aInfoChanIn, aInfoChanOut) <- newChan 32
            --descrDB   <- connectOrRecoveryConnect
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
            (aFileChan, aOutFileRequestChan) <- newChan 16
            void $ C.forkIO $ startDataActor aOutFileRequestChan
            void $ C.forkIO $ bootNodeServer poa_p aInfoChanIn aFileChan
            void $ C.forkIO $ serveInfoMsg (ConnectInfo stat_h stat_p) (ConnectInfo logs_h logs_p) aInfoChanOut log_id

            forever $ C.threadDelay 10000000000
