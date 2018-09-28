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

    -- TODO: make this more correct.
    -- TODO: use bracket idiom here

initialize :: Config -> IO ()
initialize config = do
    putStrLn @Text "Getting log file..."
    appLog <- appFileName
    putStrLn @Text $ "Log file: " +| appLog |+ "."

    let loggerConfig' = (loggerConfig config) & Lens.logFilePath .~ appLog

    putStrLn @Text "Creating logger runtime..."
    loggerRt <- createLoggerRuntime True loggerConfig'
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
    threadDelay $ 1000 * 1000 * 1000

    putStrLn @Text "Clearing node runtime..."
    clearNodeRuntime nodeRt
    putStrLn @Text "Clearing core runtime..."
    clearCoreRuntime coreRt
    putStrLn @Text "Clearing logger runtime..."
    clearLoggerRuntime loggerRt


dispatchScenario :: Config -> NodeRuntime -> ScenarioNode -> IO ()
dispatchScenario config nodeRt (ScenarioNode BootNode _ _) = runNodeDefinitionL nodeRt $ S.bootNode config
dispatchScenario config nodeRt (ScenarioNode MasterNode _ _) = runNodeDefinitionL nodeRt $ S.masterNode config
dispatchScenario _ nodeRt (ScenarioNode NetworkNode LedgerBalance Respondent) = do
    graph <- TG.initGraph
    runNodeDefinitionL nodeRt $ S.networkNode2 graph
dispatchScenario _ nodeRt (ScenarioNode NetworkNode LedgerBalance Interviewer) = runNodeDefinitionL nodeRt S.networkNode1
dispatchScenario _ nodeRt (ScenarioNode NetworkNode Sync Respondent)  = runNodeDefinitionL nodeRt S.networkNode3
dispatchScenario _ nodeRt (ScenarioNode NetworkNode Sync Interviewer) = runNodeDefinitionL nodeRt $ S.networkNode4
