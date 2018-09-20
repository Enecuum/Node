
module Enecuum.Core.LoggerSpec where

import           Enecuum.Prelude
import           System.Directory
import           Test.Hspec

import           Enecuum.Assets.System.Directory              (appFileName, defaultLogFileName)
import           Enecuum.Core.Logger.Config                   (logConfig)
import qualified Enecuum.Core.Logger.Language                 as L
import qualified Enecuum.Core.Logger.Impl.HsLogger            as Impl
import qualified Enecuum.Core.Types                           as T

scenario :: L.LoggerL ()
scenario = do
    L.logMessage T.Debug "Debug Msg"
    L.logMessage T.Info "Info Msg"
    L.logMessage T.Warning "Warning Msg"
    L.logMessage T.Error "Error Msg"

-- | Idempotent function: write log to file -- TODO change to tempFile
withLogFile
  :: FilePath            -- ^ The filepath to log
  -> IO a                -- ^ The real writeLog Function
  -> IO Text             -- ^ The content of log
withLogFile logFile action = do
  fileExists <- doesFileExist logFile
  when fileExists $ removeFile logFile
  _ <- action
  content <- readFile logFile
  removeFile logFile
  pure content

spec :: Spec
spec = do
  describe "Logger tests" $ do
    it "Debug level" $ do
      logFile <- defaultLogFileName
      res <- withLogFile logFile
              $ Impl.withLogger T.nullFormat logFile T.Debug
              $ Impl.runLoggerL scenario
      res `shouldBe` "Debug Msg\nInfo Msg\nWarning Msg\nError Msg\n"

    it "Error level" $ do
      logFile <- defaultLogFileName
      res <- withLogFile logFile
              $ Impl.withLogger T.nullFormat logFile T.Error
              $ Impl.runLoggerL scenario
      res `shouldBe` "Error Msg\n"

    -- it "Logging with package hslogger. \
    --    \Generate log file with config, set level - Debug (msg > Debug)" $ do
    --   (T.LoggerConfig format level filePath) <- logConfig
    --   logFileConetnt <- writeLog (loggerTestSet level format) filePath
    --   logFileConetnt `shouldBe` "Debug Msg\nInfo Msg\nWarning Msg\nError Msg\n"
    --
    -- it "Logging with package hslogger. \
    --    \Generate log file, set level - Debug (msg > Debug)" $ do
    --   logFileConetnt <- writeLog (loggerTestSet T.Debug T.nullFormat) =<< defaultLogFileName
    --   logFileConetnt `shouldBe` "Debug Msg\nInfo Msg\nWarning Msg\nError Msg\n"
    --
    -- it "Logging with package hslogger. \
    --    \Generate log file, \
    --    \set level - Info (msg > Info)" $ do
    --   logFileConetnt <- writeLog (loggerTestSet T.Info T.nullFormat) =<< defaultLogFileName
    --   logFileConetnt `shouldBe` "Info Msg\nWarning Msg\nError Msg\n"
    --
    -- it "Logging with package hslogger. \
    --    \Generate log file, \
    --    \set level - Debug, set format - '$prio $loggername: $msg'" $ do
    --   logFileConetnt <- writeLog (loggerTestSet T.Debug T.standartFormat) =<< defaultLogFileName
    --   logFileConetnt `shouldBe` "DEBUG LoggingExample.Main: Debug Msg\n\
    --                             \INFO LoggingExample.Main: Info Msg\n\
    --                             \WARNING LoggingExample.Main: Warning Msg\n\
    --                             \ERROR LoggingExample.Main: Error Msg\n"
    --
    -- it "Logging with package hslogger. \
    --    \Generate log file, set filepath" $ do
    --   logFileConetnt <- writeLog (loggerTestSet T.Debug T.nullFormat) =<< appFileName
    --   logFileConetnt `shouldBe` "Debug Msg\nInfo Msg\nWarning Msg\nError Msg\n"
    --
    -- it "Logging with package hslogger. \
    --    \Generate log file, set filepath, set format, set level" $ do
    --   logFileConetnt <- writeLog (loggerTestSet T.Info T.standartFormat) =<< appFileName
    --   logFileConetnt `shouldBe` "INFO LoggingExample.Main: Info Msg\n\
    --                             \WARNING LoggingExample.Main: Warning Msg\n\
    --                             \ERROR LoggingExample.Main: Error Msg\n"
