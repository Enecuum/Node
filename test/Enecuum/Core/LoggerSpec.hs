
module Enecuum.Core.LoggerSpec where

import           Enecuum.Prelude
import           System.Directory
import           Test.Hspec

import           Enecuum.Assets.System.Directory   (defaultLogFileName)
import           Enecuum.Core.Logger.Config        (logConfig)
import qualified Enecuum.Core.Logger.Impl.HsLogger as Impl
import qualified Enecuum.Core.Logger.Language      as L
import qualified Enecuum.Core.Runtime              as R
import qualified Enecuum.Core.Types                as T

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
    it "Set level, filepath, format via config" $ do
      (T.LoggerConfig format level logFile) <- logConfig
      res <- withLogFile logFile
              $ Impl.withLogger format logFile level
              $ Impl.runLoggerL scenario
      res `shouldBe` "Debug Msg\nInfo Msg\nWarning Msg\nError Msg\n"

    it "Set level: Debug level" $ do
      logFile <- defaultLogFileName
      res <- withLogFile logFile
              $ Impl.withLogger T.nullFormat logFile T.Debug
              $ Impl.runLoggerL scenario
      res `shouldBe` "Debug Msg\nInfo Msg\nWarning Msg\nError Msg\n"

    it "Set level: Error level" $ do
      logFile <- defaultLogFileName
      res <- withLogFile logFile
              $ Impl.withLogger T.nullFormat logFile T.Error
              $ Impl.runLoggerL scenario
      res `shouldBe` "Error Msg\n"

    it "Set format: '$prio $loggername: $msg'" $ do
      logFile <- defaultLogFileName
      res <- withLogFile logFile
              $ Impl.withLogger T.standartFormat logFile T.Debug
              $ Impl.runLoggerL scenario
      res `shouldBe` "DEBUG Node.Main: Debug Msg\n\
                     \INFO Node.Main: Info Msg\n\
                     \WARNING Node.Main: Warning Msg\n\
                     \ERROR Node.Main: Error Msg\n"
