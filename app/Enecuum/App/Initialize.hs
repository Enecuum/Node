module App.Initialize where

import qualified Data.Map                        as M
import           Enecuum.Prelude

import qualified Enecuum.Assets.Scenarios        as S
import           Enecuum.Assets.System.Directory (clientStory)
import           Enecuum.Config                  (Config (..), NodeRole (..), Scenario (..), ScenarioNode (..),
                                                  ScenarioRole (..))
import qualified Enecuum.Core.Lens               as Lens
import           Enecuum.Interpreters            (clearNodeRuntime, runNodeDefinitionL)
import qualified Enecuum.Language                as L
import           Enecuum.Runtime                 (clearCoreRuntime, clearLoggerRuntime,
                                                  createCoreRuntime, createLoggerRuntime, createNodeRuntime)


    -- TODO: make this more correct.
    -- TODO: use bracket idiom here

initialize :: Config -> IO ()
initialize config = do
    putStrLn @Text "Getting log file..."
    let loggerConfig' = loggerConfig config
    let logFile       = loggerConfig' ^. Lens.logFilePath
    putStrLn @Text $ "Log file: " +| logFile |+ "."

    putStrLn @Text "Creating logger runtime..."
    loggerRt <- createLoggerRuntime loggerConfig'
    putStrLn @Text "Creating core runtime..."
    coreRt <- createCoreRuntime loggerRt
    putStrLn @Text "Creating node runtime..."
    story <- clientStory
    nodeRt <- createNodeRuntime coreRt (M.singleton "Client" story)

    forM_ (scenarioNode config) $ \scenarioCase -> runNodeDefinitionL nodeRt $ do
        L.logInfo
            $   "Starting node.\n  Role: " +|| nodeRole scenarioCase
            ||+ "\n  Scenario: " +|| scenario scenarioCase
            ||+ "\n  Case: " +|| scenarioRole scenarioCase ||+ "..."
        dispatchScenario config scenarioCase

    putStrLn @Text "Clearing node runtime..."
    clearNodeRuntime nodeRt
    putStrLn @Text "Clearing core runtime..."
    clearCoreRuntime coreRt
    putStrLn @Text "Clearing logger runtime..."
    clearLoggerRuntime loggerRt

dispatchScenario :: Config -> ScenarioNode -> L.NodeDefinitionL ()
dispatchScenario config (ScenarioNode BootNode    _         _           ) = S.bootNode config
dispatchScenario config (ScenarioNode MasterNode  _         _           ) = S.masterNode config
dispatchScenario _      (ScenarioNode Client      _         _           ) = S.clientNode
dispatchScenario _      (ScenarioNode NetworkNode SyncChain Respondent  ) = S.networkNode3
dispatchScenario _      (ScenarioNode NetworkNode SyncChain Interviewer ) = S.networkNode4
dispatchScenario _      (ScenarioNode PoW         Full      Soly        ) = S.powNode
dispatchScenario _      (ScenarioNode PoA         Full      role        ) = S.poaNode role
dispatchScenario _      (ScenarioNode NetworkNode Full      Soly        ) = S.nnNode
dispatchScenario _      (ScenarioNode GraphNodeTransmitter _  _         ) = S.graphNodeTransmitter
dispatchScenario _      (ScenarioNode GraphNodeReceiver   _    _        ) = S.graphNodeReceiver
dispatchScenario _      (ScenarioNode role        scenario  scenarioRole) = error mes
    where mes = "This scenario: " +|| role ||+ scenario ||+ scenarioRole ||+ " doesn't exist"
