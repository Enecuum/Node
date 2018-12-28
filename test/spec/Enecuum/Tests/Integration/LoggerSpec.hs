
module Enecuum.Tests.Integration.LoggerSpec where

import           Data.Text                         (unpack)
import qualified Enecuum.Core.Lens                 as Lens
import qualified Enecuum.Core.Logger.Impl.HsLogger as Impl
import qualified Enecuum.Core.Logger.Language      as L
import qualified Enecuum.Core.Types                as T
import           Enecuum.Interpreters              (runFileSystemL)
import           Enecuum.Prelude                   hiding (unpack)
import           Enecuum.Testing.Integrational
import           Enecuum.Testing.Wrappers
import           Enecuum.Tests.Helpers
import           System.Directory
import           System.IO.Silently                (capture)
import           Test.Hspec

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
spec =
    stableTest $ fastTest $ describe "Logger tests" $ do
        it "Test output to console with capture" $ do
            config      <- loadLoggerConfig testConfigFilePath
            (output, _) <- capture $ Impl.withLogger config { T._logToConsole = True, T._logToFile = False } $ \h ->
                Impl.runLoggerL (Just h) scenario
            output `shouldBe` (unpack standardFormattedFullText)

        -- TODO: FIXME: better tests with resources cleanup
        -- it "Switch off for logging to file" $ do
        --     res <- logViaConfig False False
        --     res `shouldBe` ""

        it "Set level, filepath, format via config" $ do
            res <- logViaConfig False True
            res `shouldBe` standardFormattedFullText

        it "Set level: Debug level" $ do
            res <- logViaDefault T.Debug T.nullFormat
            res `shouldBe` "Debug Msg\nInfo Msg\nWarning Msg\nError Msg\n"

        it "Set level: Error level" $ do
            res <- logViaDefault T.Error T.nullFormat
            res `shouldBe` "Error Msg\n"

        it "Set format: '$prio $loggername: $msg'" $ do
            res <- logViaDefault T.Debug T.standardFormat
            res `shouldBe` standardFormattedFullText

logViaDefault level format = do
    logFile <- runFileSystemL $ pure testLogFilePath
    let config = T.LoggerConfig format level logFile False True
    runLog logFile config

logViaConfig logToConsole logToFile = do
    config      <- loadLoggerConfig testConfigFilePath
    let logFile = config ^. Lens.logFilePath
    runLog logFile config { T._logToConsole = logToConsole, T._logToFile = logToFile }

runLog logFile config =
    withLogFile logFile $ Impl.withLogger config $ \h ->
        Impl.runLoggerL (Just h) scenario

standardFormattedFullText :: Text
standardFormattedFullText = "DEBUG : Debug Msg\n\
                            \INFO : Info Msg\n\
                            \WARNING : Warning Msg\n\
                            \ERROR : Error Msg\n"
