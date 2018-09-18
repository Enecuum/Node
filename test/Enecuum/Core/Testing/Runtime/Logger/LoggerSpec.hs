module Enecuum.Core.Testing.Runtime.Logger.LoggerSpec where

import           Enecuum.Core.System.Directory            (appFileName,
                                                           defaultLogFileName)
import           Enecuum.Core.Testing.Runtime.Logger.Impl
import qualified Enecuum.Core.Types.Logger                as T
import           Enecuum.Prelude
import           System.Directory
import           Test.Hspec

logFile :: IO a -> FilePath -> IO Text
logFile doIO logFile = do
  fileExists <- doesFileExist logFile
  when fileExists $ removeFile logFile
  doIO
  content <- readFile logFile
  removeFile logFile
  pure content

getLog fun = logFile fun =<< defaultLogFileName

spec :: Spec
spec = do
  describe "Logger tests" $ do
    it "Generate log file without config" $ do
      logFileConetnt <- getLog loggerTestWithoutConfig
      logFileConetnt `shouldBe` "Debug Msg\nInfo Msg\nWarning Msg\nError Msg\n"

    it "Generate log file with config, set level - Debug (msg > Debug)" $ do
      logFileConetnt <- getLog $ loggerTestSet T.Debug ""
      logFileConetnt `shouldBe` "Debug Msg\nInfo Msg\nWarning Msg\nError Msg\n"

    it "Generate log file with config, set level - Info (msg > Info)" $ do
      logFileConetnt <- getLog $ loggerTestSet T.Info ""
      logFileConetnt `shouldBe` "Info Msg\nWarning Msg\nError Msg\n"

    it "Generate log file with config, set level - Debug, set format - '$prio $loggername: $msg'" $ do
      logFileConetnt <- getLog $ loggerTestSet T.Debug T.standartFormat
      logFileConetnt `shouldBe` "DEBUG LoggingExample.Main: Debug Msg\nINFO LoggingExample.Main: Info Msg\nWARNING LoggingExample.Main: Warning Msg\nERROR LoggingExample.Main: Error Msg"

    it "Generate log file with config, set filepath" $ do
      logFileConetnt <- logFile (loggerTestSet T.Debug "") =<< appFileName
      logFileConetnt `shouldBe` "Debug Msg\nInfo Msg\nWarning Msg\nError Msg\n"
