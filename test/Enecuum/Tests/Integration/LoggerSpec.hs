
module Enecuum.Tests.Integration.LoggerSpec where

import           Enecuum.Prelude hiding (unpack)
import           System.Directory
import           Test.Hspec

import           Enecuum.Interpreters            (runFileSystemL)
import           Enecuum.Assets.System.Directory   (defaultLogFileName, configFilePath)
import qualified Enecuum.Core.Lens              as Lens
import qualified Enecuum.Core.Logger.Impl.HsLogger as Impl
import qualified Enecuum.Core.Logger.Language      as L
import qualified Enecuum.Core.Types                as T
import Enecuum.Config (logConfig)
import System.IO.Silently
import Data.Text (unpack)

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
            (output, _) <- capture $ Impl.withLogger config { T._logToConsole = True, T._logToFile = False } $ \h ->
                Impl.runLoggerL (Just h) scenario
            output `shouldBe` (unpack standartFormattedFullText)
            
        it "Switch off for logging to file" $ do
            res <- logViaConfig False False
            res `shouldBe` ""

        it "Set level, filepath, format via config" $ do
            res <- logViaConfig False True
            res `shouldBe` standartFormattedFullText

        it "Set level: Debug level" $ do
            res <- logViaDefault T.Debug T.nullFormat
            res `shouldBe` "Debug Msg\nInfo Msg\nWarning Msg\nError Msg\n"

        it "Set level: Error level" $ do
            res <- logViaDefault T.Error T.nullFormat
            res `shouldBe` "Error Msg\n"

        it "Set format: '$prio $loggername: $msg'" $ do
            res <- logViaDefault T.Debug T.standartFormat
            res `shouldBe` standartFormattedFullText



logViaDefault level format = do
    logFile <- runFileSystemL $ defaultLogFileName
    let config = T.LoggerConfig format level logFile False True
    runLog logFile config         

logViaConfig logToConsole logToFile = do    
    config      <- logConfig configFilePath
    let logFile = config ^. Lens.logFilePath
    runLog logFile config { T._logToConsole = logToConsole, T._logToFile = logToFile }

runLog logFile config =     
    withLogFile logFile $ Impl.withLogger config $ \h ->
        Impl.runLoggerL (Just h) scenario    

standartFormattedFullText :: Text        
standartFormattedFullText = "DEBUG Node.Main: Debug Msg\n\
                            \INFO Node.Main: Info Msg\n\
                            \WARNING Node.Main: Warning Msg\n\
                            \ERROR Node.Main: Error Msg\n"        