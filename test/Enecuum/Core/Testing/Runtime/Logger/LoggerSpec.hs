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
  _ <- doIO
  content <- readFile logFile
  removeFile logFile
  pure content

getLog :: (FilePath -> IO a) -> FilePath -> IO Text
getLog  fun filepath = logFile (fun filepath) filepath

spec :: Spec
spec = do
  describe "Logger tests" $ do
    it "Logging with package hslogger. \
       \Generate log file without config" $ do
      logFileConetnt <- logFile loggerTestWithoutConfig =<< defaultLogFileName
      logFileConetnt `shouldBe` "Debug Msg\nInfo Msg\nWarning Msg\nError Msg\n"

    it "Logging with package hslogger. \
       \Generate log file with config, set level - Debug (msg > Debug)" $ do
      logFileConetnt <- getLog (loggerTestSet T.Debug T.nullFormat) =<< defaultLogFileName
      logFileConetnt `shouldBe` "Debug Msg\nInfo Msg\nWarning Msg\nError Msg\n"

    it "Logging with package hslogger. \
       \Generate log file with config, \
       \set level - Info (msg > Info)" $ do
      logFileConetnt <- getLog (loggerTestSet T.Info T.nullFormat) =<< defaultLogFileName
      logFileConetnt `shouldBe` "Info Msg\nWarning Msg\nError Msg\n"

    it "Logging with package hslogger. \
       \Generate log file with config, \
       \set level - Debug, set format - '$prio $loggername: $msg'" $ do
      logFileConetnt <- getLog (loggerTestSet T.Debug T.standartFormat) =<< defaultLogFileName
      logFileConetnt `shouldBe` "DEBUG LoggingExample.Main: Debug Msg\n\
                                \INFO LoggingExample.Main: Info Msg\n\
                                \WARNING LoggingExample.Main: Warning Msg\n\
                                \ERROR LoggingExample.Main: Error Msg\n"

    it "Logging with package hslogger. \
       \Generate log file with config, set filepath" $ do
      logFileConetnt <- getLog (loggerTestSet T.Debug T.nullFormat) =<< appFileName
      logFileConetnt `shouldBe` "Debug Msg\nInfo Msg\nWarning Msg\nError Msg\n"

    it "Logging with package hslogger. \
       \Generate log file with config, set filepath, set format, set level" $ do
      logFileConetnt <- getLog (loggerTestSet T.Info T.standartFormat) =<< appFileName
      logFileConetnt `shouldBe` "INFO LoggingExample.Main: Info Msg\n\
                                \WARNING LoggingExample.Main: Warning Msg\n\
                                \ERROR LoggingExample.Main: Error Msg\n"
