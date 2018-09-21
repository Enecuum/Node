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

    -- TODO: make this more correct.
    -- TODO: use bracket idiom here

initialize :: Config -> IO ()
initialize config = do
    putStrLn @Text "Getting log file..."
    appLog <- appFileName
    putStrLn @Text $ "Log file: " +| appLog |+ "."

    let loggerConfig' = (loggerConfig config) & Lens.logFilePath .~ appLog

    putStrLn @Text "Creating logger runtime..."
    loggerRt <- createLoggerRuntime loggerConfig'
    putStrLn @Text "Creating core runtime..."
    coreRt <- createCoreRuntime loggerRt
    putStrLn @Text "Creating node runtime..."
    nodeRt <- createNodeRuntime coreRt

    putStrLn @Text "Starting node..."
    -- when (bootNode config)   $ runNodeDefinitionL nodeRt $ S.bootNode   config
    -- when (masterNode config) $ runNodeDefinitionL nodeRt $ S.masterNode config

    putStrLn @Text "Clearing node runtime..."
    clearNodeRuntime nodeRt
    putStrLn @Text "Clearing core runtime..."
    clearCoreRuntime coreRt
    putStrLn @Text "Clearing logger runtime..."
    clearLoggerRuntime loggerRt
