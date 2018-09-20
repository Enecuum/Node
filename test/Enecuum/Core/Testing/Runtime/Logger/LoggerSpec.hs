module Enecuum.Core.Testing.Runtime.Logger.LoggerSpec where

-- import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Concurrent.Chan
import           Enecuum.Core.Logger.Config                   (logConfig)
import           Enecuum.Core.Logger.Language
import           Enecuum.Core.System.Directory                (appFileName, defaultLogFileName)
import           Enecuum.Core.Testing.Runtime.Logger.ImplFile (runLoggerL)
import           Enecuum.Core.Testing.Runtime.Types           (createLoggerRuntimeFile)
import qualified Enecuum.Core.Types.Logger                    as T
import           Enecuum.Prelude
import           System.Directory
import           Test.Hspec

loggerTest = do
    -- setupFile T.Debug "log" T.standartFormat
    logMessage T.Debug "Debug Msg"
    logMessage T.Info "Info Msg"
    logMessage T.Warning "Warning Msg"
    logMessage T.Error "Error Msg"

-- | Idempotent function: write log to file -- TODO change to tempFile
writeLog :: (FilePath -> IO a)  -- ^ The real writeLog Function
         -> FilePath            -- ^ The filepath to log
         -> IO Text             -- ^ The content of log
writeLog doIO logFile = do
  fileExists <- doesFileExist logFile
  when fileExists $ removeFile logFile
  _ <- doIO logFile
  content <- readFile logFile
  removeFile logFile
  pure content

-- | Initialize Logger Runtime and run test
loggerTestSet :: T.LogLevel -> T.Format -> FilePath -> IO ()
loggerTestSet level format filePath = do
  fileChan <- newChan
  loggerRuntime <- createLoggerRuntimeFile fileChan (T.LoggerConfig format level filePath)
  -- setupFile level filePath format
  -- forkIO $ forever $ do
  --   content <- getChanContents fileChan
  --   print "hello"
    --write to file
  runLoggerL loggerRuntime loggerTest

spec :: Spec
spec = do
  describe "Logger tests" $ do
    it "Logging with package hslogger. \
       \Generate log file with config, set level - Debug (msg > Debug)" $ do
      (T.LoggerConfig format level filePath) <- logConfig
      logFileConetnt <- writeLog (loggerTestSet level format) filePath
      logFileConetnt `shouldBe` "Debug Msg\nInfo Msg\nWarning Msg\nError Msg\n"

    it "Logging with package hslogger. \
       \Generate log file, set level - Debug (msg > Debug)" $ do
      logFileConetnt <- writeLog (loggerTestSet T.Debug T.nullFormat) =<< defaultLogFileName
      logFileConetnt `shouldBe` "Debug Msg\nInfo Msg\nWarning Msg\nError Msg\n"

    it "Logging with package hslogger. \
       \Generate log file, \
       \set level - Info (msg > Info)" $ do
      logFileConetnt <- writeLog (loggerTestSet T.Info T.nullFormat) =<< defaultLogFileName
      logFileConetnt `shouldBe` "Info Msg\nWarning Msg\nError Msg\n"

    it "Logging with package hslogger. \
       \Generate log file, \
       \set level - Debug, set format - '$prio $loggername: $msg'" $ do
      logFileConetnt <- writeLog (loggerTestSet T.Debug T.standartFormat) =<< defaultLogFileName
      logFileConetnt `shouldBe` "DEBUG LoggingExample.Main: Debug Msg\n\
                                \INFO LoggingExample.Main: Info Msg\n\
                                \WARNING LoggingExample.Main: Warning Msg\n\
                                \ERROR LoggingExample.Main: Error Msg\n"

    it "Logging with package hslogger. \
       \Generate log file, set filepath" $ do
      logFileConetnt <- writeLog (loggerTestSet T.Debug T.nullFormat) =<< appFileName
      logFileConetnt `shouldBe` "Debug Msg\nInfo Msg\nWarning Msg\nError Msg\n"

    it "Logging with package hslogger. \
       \Generate log file, set filepath, set format, set level" $ do
      logFileConetnt <- writeLog (loggerTestSet T.Info T.standartFormat) =<< appFileName
      logFileConetnt `shouldBe` "INFO LoggingExample.Main: Info Msg\n\
                                \WARNING LoggingExample.Main: Warning Msg\n\
                                \ERROR LoggingExample.Main: Error Msg\n"
