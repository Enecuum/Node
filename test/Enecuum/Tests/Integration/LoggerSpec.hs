
module Enecuum.Tests.Integration.LoggerSpec where

import           Enecuum.Prelude
import           System.Directory
import           Test.Hspec

import           Enecuum.Interpreters            (runFileSystemL)
import           Enecuum.Assets.System.Directory   (defaultLogFileName, configFilePath)
import qualified Enecuum.Core.Logger.Impl.HsLogger as Impl
import qualified Enecuum.Core.Logger.Language      as L
import qualified Enecuum.Core.Runtime              as R
import qualified Enecuum.Core.Types                as T
import qualified Enecuum.Core.Lens as Lens
import Enecuum.Config (logConfig)
import System.IO.Silently

scenario :: L.LoggerL ()
scenario = do
    L.logMessage T.Debug   "Debug Msg"
    L.logMessage T.Info    "Info Msg"
    L.logMessage T.Warning "Warning Msg"
    L.logMessage T.Error   "Error Msg"

-- | Idempotent function: write log to file -- TODO change to tempFile
withLogFile
    :: FilePath            -- ^ The filepath to log
    -> IO a                -- ^ The real writeLog Function
    -> IO Text             -- ^ The content of log
withLogFile logFile action = do
    fileExists <- doesFileExist logFile
    when fileExists $ removeFile logFile
    _       <- action
    content <- readFile logFile
    removeFile logFile
    pure content

spec :: Spec
spec = do
    describe "Logger tests" $ do
        it "Test output to console with capture" $ do
            config      <- logConfig configFilePath
            (output, _) <- capture $ Impl.withLogger config { T._logToConsole = True } $ \h ->
                Impl.runLoggerL (Just h) scenario
            output
                `shouldBe` "DEBUG Node.Main: Debug Msg\n\
                        \INFO Node.Main: Info Msg\n\
                        \WARNING Node.Main: Warning Msg\n\
                        \ERROR Node.Main: Error Msg\n"

        it "Set level, filepath, format via config" $ do
            config@(T.LoggerConfig _ _ logFile _) <- logConfig configFilePath
            res <- withLogFile logFile $ Impl.withLogger config { T._logToConsole = False } $ \h ->
                Impl.runLoggerL (Just h) scenario
            res
                `shouldBe` "DEBUG Node.Main: Debug Msg\n\
                     \INFO Node.Main: Info Msg\n\
                     \WARNING Node.Main: Warning Msg\n\
                     \ERROR Node.Main: Error Msg\n"

        it "Set level: Debug level" $ do
            logFile <- runFileSystemL $ defaultLogFileName
            let config = T.LoggerConfig T.nullFormat T.Debug logFile False
            res <- withLogFile logFile $ Impl.withLogger config $ \h -> Impl.runLoggerL (Just h) scenario
            res `shouldBe` "Debug Msg\nInfo Msg\nWarning Msg\nError Msg\n"

        it "Set level: Error level" $ do
            logFile <- runFileSystemL $ defaultLogFileName
            let config = T.LoggerConfig T.nullFormat T.Error logFile False
            res <- withLogFile logFile $ Impl.withLogger config $ \h -> Impl.runLoggerL (Just h) scenario
            res `shouldBe` "Error Msg\n"

        it "Set format: '$prio $loggername: $msg'" $ do
            logFile <- runFileSystemL $ defaultLogFileName
            let config = T.LoggerConfig T.standartFormat T.Debug logFile False
            res <- withLogFile logFile $ Impl.withLogger config $ \h -> Impl.runLoggerL (Just h) scenario
            res
                `shouldBe` "DEBUG Node.Main: Debug Msg\n\
                     \INFO Node.Main: Info Msg\n\
                     \WARNING Node.Main: Warning Msg\n\
                     \ERROR Node.Main: Error Msg\n"
