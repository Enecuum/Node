module Enecuum.Core.Testing.Runtime.Logger.LoggerSpec where

import           Enecuum.Core.System.Directory            (appFilename)
import           Enecuum.Core.Testing.Runtime.Logger.Impl
import qualified Enecuum.Core.Types.Logger                as T
import           Enecuum.Prelude
import           System.Directory
import           Test.Hspec


logFile fun = do
      fileExists <- doesFileExist appFilename
      when fileExists $ removeFile appFilename
      fun
      readFile appFilename

spec :: Spec
spec = do
  describe "Logger tests" $ do
    -- it "Generate log file without config" $ do
    --   logFileConetnt <- logFile loggerTestWithoutConfig
    --   logFileConetnt `shouldBe` "Debug Msg\nInfo Msg\nWarning Msg\nError Msg\n"
    it "Generate log file with config: msg > Debug" $ do
      logFileConetnt <- logFile $ loggerTestWithConfig T.Debug
      logFileConetnt `shouldBe` "Debug Msg\nInfo Msg\nWarning Msg\nError Msg\n"
    it "Generate log file with config: msg > Info" $ do
      logFileConetnt <- logFile $ loggerTestWithConfig T.Info
      logFileConetnt `shouldBe` "Info Msg\nWarning Msg\nError Msg\n"
