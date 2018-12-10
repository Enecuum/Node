{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Enecuum.Tests.Helpers where

import           Control.Concurrent                               (killThread)
import           Data.Aeson
import qualified Data.ByteString.Lazy                             as LBS
import qualified Data.Map                                         as M
import           Data.Yaml
import qualified "rocksdb-haskell" Database.RocksDB               as Rocks
import qualified Enecuum.Config                                   as Cfg
import qualified Enecuum.Core.Lens                                as Lens
import qualified Enecuum.Core.RLens                               as RLens
import qualified Enecuum.Domain                                   as D
import qualified Enecuum.Framework.NodeDefinition.Interpreter     as R
import qualified Enecuum.Framework.RLens                          as RLens
import qualified Enecuum.Interpreters                             as I
import qualified Enecuum.Language                                 as L
import           Enecuum.Prelude
import qualified Enecuum.Runtime                                  as R
import           Enecuum.Samples.Assets.Nodes.Address             as A
import           Enecuum.Samples.Assets.Nodes.Client              (ClientNode)
import qualified Enecuum.Samples.Assets.Nodes.GraphService.Config as Cfg
import           Enecuum.Samples.Assets.Nodes.Messages            as D
import qualified Enecuum.Samples.Assets.TstScenarios              as Tst
import qualified Enecuum.Samples.Blockchain.Domain                as D
import qualified Enecuum.Samples.Blockchain.Language              as L
import           Enecuum.Testing.Integrational
import qualified System.Directory                                 as Dir
import           System.FilePath                                  as FP ((</>))
import qualified System.FilePath                                  as Dir

loadLoggerConfig :: FilePath -> IO D.LoggerConfig
loadLoggerConfig configFile = do
    configSrc <- LBS.readFile configFile
    case (Cfg.tryParseConfig @ClientNode configSrc) of
        Left e       -> (error . show . prettyPrintParseException) $ e
        Right config -> doSomethingWithConfig config

doSomethingWithConfig :: D.Config ClientNode -> IO D.LoggerConfig
doSomethingWithConfig cfg = do
        let logConf = Cfg.loggerConfig cfg
        let dir = Dir.dropFileName $ logConf ^. Lens.logFilePath
        Dir.createDirectoryIfMissing True dir
        pure logConf

waitForBlocks2 :: D.BlockNumber -> D.Address -> IO ()
waitForBlocks2 number address = do
    void $ makeRpcRequestWithPredicate predicate address request
    where request = D.GetChainLengthRequest
          predicate = \(D.GetChainLengthResponse count) -> count < (fromIntegral number)

waitForBlocks' :: Word32 -> D.BlockNumber -> D.Address -> IO ()
waitForBlocks' attempts number address = go 0
    where
        go :: D.BlockNumber -> IO ()
        go n | n == attempts = error "No valid results from node."
        go n = do
            threadDelay $ 1000 * 100
            D.GetChainLengthResponse count <- D.withSuccess $ makeIORpcRequest address D.GetChainLengthRequest
            when (count < number) $ go (n + 1)

waitForBlocks :: D.BlockNumber -> D.Address -> IO ()
waitForBlocks = waitForBlocks' 50
