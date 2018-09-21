module App.Initialize where

import           Enecuum.Prelude

import           Enecuum.Config  (Config(..))
import qualified Enecuum.Core.Lens as Lens
import qualified Enecuum.Assets.Scenarios as S
import           Enecuum.Assets.System.Directory (appFileName)
import           Enecuum.Interpreters (runNodeDefinitionL)
import           Enecuum.Runtime (createNodeRuntime, createLoggerRuntime,
                                  clearNodeRuntime, clearLoggerRuntime,
                                  createCoreRuntime, clearCoreRuntime)

initialize :: Config -> IO ()
initialize config = do
    -- TODO: make logger loading correctly
    -- TODO: use bracket idiom here
    appLog <- appFileName

    let loggerConfig' = (loggerConfig config) & Lens.logFilePath .~ appLog

    loggerRt <- createLoggerRuntime loggerConfig'
    coreRt <- createCoreRuntime loggerRt
    nodeRt <- createNodeRuntime coreRt

    when (bootNode config)   $ runNodeDefinitionL nodeRt $ S.bootNode   config
    when (masterNode config) $ runNodeDefinitionL nodeRt $ S.masterNode config

    clearNodeRuntime nodeRt
    clearCoreRuntime coreRt
    clearLoggerRuntime loggerRt
