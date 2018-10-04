module App.Initialize where

import           Enecuum.Prelude

import           Enecuum.Config  (Config(..), ScenarioNode(..),NodeRole(..), Scenario(..), ScenarioRole(..))
import qualified Enecuum.Core.Lens as Lens
import qualified Enecuum.Assets.Scenarios as S
import           Enecuum.Assets.System.Directory (appFileName)
import           Enecuum.Interpreters (runNodeDefinitionL)
import           Enecuum.Runtime (NodeRuntime(..), createNodeRuntime, createLoggerRuntime,
                                  clearNodeRuntime, clearLoggerRuntime,
                                  createCoreRuntime, clearCoreRuntime)
import qualified Enecuum.Blockchain.Domain.Graph as TG
import qualified Enecuum.Framework.RLens as Lens
    -- TODO: make this more correct.
    -- TODO: use bracket idiom here

initialize :: Config -> IO ()
initialize config = do
    putStrLn @Text "Getting log file..."
    let loggerConfig' = (loggerConfig config)
    let logFile = loggerConfig' ^. Lens.logFilePath
    putStrLn @Text $ "Log file: " +| logFile |+ "."

    putStrLn @Text "Creating logger runtime..."
    loggerRt <- createLoggerRuntime $ loggerConfig'
    putStrLn @Text "Creating core runtime..."
    coreRt <- createCoreRuntime loggerRt
    putStrLn @Text "Creating node runtime..."
    nodeRt <- createNodeRuntime coreRt
    forM_ (scenarioNode config) $ (\scenarioCase-> forkIO $ do
        putStrLn $ "Starting " ++ (show $ nodeRole scenarioCase) ++ " node..."
        putStrLn $ "Starting " ++ (show $ scenario scenarioCase) ++ " scenario..."
        putStrLn $ "Starting " ++ (show $ scenarioRole scenarioCase) ++ " role..."
        dispatchScenario config nodeRt scenarioCase)
        -- TODO: this is a quick hack. Make it right.
    atomically $ readTMVar (nodeRt ^. Lens.stopNode)

    putStrLn @Text "Clearing node runtime..."
    clearNodeRuntime nodeRt
    putStrLn @Text "Clearing core runtime..."
    clearCoreRuntime coreRt
    putStrLn @Text "Clearing logger runtime..."
    clearLoggerRuntime loggerRt


dispatchScenario :: Config -> NodeRuntime -> ScenarioNode -> IO ()
dispatchScenario config nodeRt (ScenarioNode BootNode _ _) = runNodeDefinitionL nodeRt $ S.bootNode config
dispatchScenario config nodeRt (ScenarioNode MasterNode _ _) = runNodeDefinitionL nodeRt $ S.masterNode config
dispatchScenario _ nodeRt (ScenarioNode NetworkNode Sync Respondent)  = runNodeDefinitionL nodeRt S.networkNode3
dispatchScenario _ nodeRt (ScenarioNode NetworkNode Sync Interviewer) = runNodeDefinitionL nodeRt $ S.networkNode4
dispatchScenario _ nodeRt (ScenarioNode PoW SyncKblock Soly) = runNodeDefinitionL nodeRt $ S.powNode
dispatchScenario _ nodeRt (ScenarioNode PoW SyncKblock Soly) = runNodeDefinitionL nodeRt $ S.poaNode
dispatchScenario _ nodeRt (ScenarioNode PoW SyncKblock Soly) = runNodeDefinitionL nodeRt $ S.nnNode
dispatchScenario _ _ (ScenarioNode role scenario scenarioRole) = 
    error $ mconcat mes 
      where mes :: [Text] = ("This scenario: " :: Text)  : (show @Text role) : (show @Text scenario) : (show @Text scenarioRole) : (" doesn't exist" :: Text) : []