{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Boot node's binaries
module Main where

import qualified Control.Concurrent                    as C
import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Monad

import           Data.Aeson
import qualified Data.ByteString.Lazy                  as L
import           Enecuum.Legacy.BootNodeServer
import           Enecuum.Legacy.Node.DataActor
import           Enecuum.Legacy.Node.Node.Types
import           Enecuum.Legacy.Service.InfoMsg
import           Enecuum.Legacy.Service.Network.Base   (ConnectInfo (..))
import           Enecuum.Prelude
import           Network.Socket                        ()
import           System.Environment


main :: IO ()
main = do
    enc <- L.readFile "configs/Legacy/config.json"
    case decode enc :: Maybe BuildConfig of
        Nothing   -> error "Please, specify config file correctly"
        Just conf -> do

            (aInfoChanIn, aInfoChanOut) <- newChan 32
            poa_p                       <- try (getEnv "poaPort") >>= \case
                Right item                 -> pure $ read item
                Left  (_ :: SomeException) -> pure $ poaPort conf

            stat_h <- try (getEnv "statsdHost") >>= \case
                Right item                 -> pure item
                Left  (_ :: SomeException) -> pure $ host $ statsdBuildConfig conf

            stat_p <- try (getEnv "statsdPort") >>= \case
                Right item                 -> pure $ read item
                Left  (_ :: SomeException) -> pure $ port $ statsdBuildConfig conf

            logs_h <- try (getEnv "logHost") >>= \case
                Right item                 -> pure item
                Left  (_ :: SomeException) -> pure $ host $ logsBuildConfig conf

            logs_p <- try (getEnv "logPort") >>= \case
                Right item                 -> pure $ read item
                Left  (_ :: SomeException) -> pure $ port $ logsBuildConfig conf

            log_id <- try (getEnv "log_id") >>= \case
                Right item                 -> pure item
                Left  (_ :: SomeException) -> pure "0"
            (aFileChan, aOutFileRequestChan) <- newChan 16
            void $ C.forkIO $ startDataActor aOutFileRequestChan
            void $ C.forkIO $ bootNodeServer poa_p aInfoChanIn aFileChan
            void $ C.forkIO $ serveInfoMsg (ConnectInfo stat_h stat_p)
                                           (ConnectInfo logs_h logs_p)
                                           aInfoChanOut
                                           log_id
                                           True

            forever $ C.threadDelay 10000000000
