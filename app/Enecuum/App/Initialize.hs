{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType   #-}
module App.Initialize where

import qualified Data.Map                        as M
import           Enecuum.Prelude

import qualified Enecuum.Assets.Scenarios        as S
import           Enecuum.Assets.System.Directory (clientStory)
import qualified Enecuum.Config                  as Cfg
import qualified Enecuum.Core.Lens               as Lens
import           Enecuum.Interpreters            (clearNodeRuntime, runFileSystemL, runNodeDefinitionL)
import qualified Enecuum.Language                as L
import qualified Enecuum.Runtime                 as R
import           Enecuum.Runtime                 (clearCoreRuntime, clearLoggerRuntime,
                                                  createCoreRuntime, createLoggerRuntime, createNodeRuntime)

createLoggerRuntime' :: Config -> IO R.LoggerRuntime
createLoggerRuntime' config = do
    let loggerConfig' = loggerConfig config
    let logFile       = loggerConfig' ^. Lens.logFilePath
    putStrLn @Text $ "Log file: " +| logFile |+ "."
    putStrLn @Text "Creating logger runtime..."
    createLoggerRuntime loggerConfig'

clearLoggerRuntime' :: R.LoggerRuntime -> IO ()
clearLoggerRuntime' loggerRt = do
    putStrLn @Text "Clearing logger runtime..."
    clearLoggerRuntime loggerRt

createCoreRuntime' :: R.LoggerRuntime -> IO R.CoreRuntime
createCoreRuntime' loggerRt = do
    putStrLn @Text "Creating core runtime..."
    createCoreRuntime loggerRt

clearCoreRuntime' :: R.CoreRuntime -> IO ()
clearCoreRuntime' coreRt = do
    putStrLn @Text "Clearing core runtime..."
    clearCoreRuntime coreRt

createNodeRuntime' :: R.CoreRuntime -> IO R.NodeRuntime
createNodeRuntime' coreRt = do
    story <- runFileSystemL clientStory
    putStrLn @Text "Creating node runtime..."
    createNodeRuntime coreRt (M.singleton "Client" story)

clearNodeRuntime' :: R.NodeRuntime -> IO ()
clearNodeRuntime' nodeRt = do
    putStrLn @Text "Clearing node runtime..."
    clearNodeRuntime nodeRt

runNode
    :: Show node
    => Cfg.NodeConfig node
    -> L.NodeDefinitionL ()
    -> R.NodeRuntime
    -> IO ()
runNode cfg node nodeRt = do
    putStrLn $
        "Starting node: " +|| node ||+
        "\n  Scenario: " +|| getScenario cfg ||+ "..."
    runNodeDefinitionL nodeRt node

initialize :: LByteString -> IO ()
initialize configSrc =
    case dispatchScenario configSrc of
        Nothing -> putStrLn @Text "Invalid config passed: node not found."
        Just (config, node) -> 
            bracket (createLoggerRuntime' config) clearLoggerRuntime' $ \loggerRt ->
            bracket (createCoreRuntime' loggerRt) clearCoreRuntime'   $ \coreRt   ->
            bracket (createNodeRuntime' coreRt)   clearNodeRuntime'   $ \nodeRt   ->
            runNode (Cfg.nodeConfig node) node nodeRt

-- initialize :: Config -> IO ()
-- initialize config =
--     bracket (createLoggerRuntime' config) clearLoggerRuntime' $ \loggerRt ->
--     bracket (createCoreRuntime' loggerRt) clearCoreRuntime'   $ \coreRt   ->
--     bracket (createNodeRuntime' coreRt)   clearNodeRuntime'   $ \nodeRt   ->
--     runNode config nodeRt

-- dispatchScenario :: Config -> ScenarioNode -> L.NodeDefinitionL ()
-- dispatchScenario _ (ScenarioNode Client      _         _           ) = S.clientNode
-- dispatchScenario _ (ScenarioNode PoW         Full      Soly        ) = S.powNode
-- dispatchScenario _ (ScenarioNode PoA         Full      role        ) = S.poaNode role
-- dispatchScenario _ (ScenarioNode GraphNode   _         Transmitter ) = S.graphNodeTransmitter
-- dispatchScenario _ (ScenarioNode GraphNode   _         Receiver    ) = S.graphNodeReceiver
-- dispatchScenario _ (ScenarioNode role        scenario  scenarioRole) = error mes
--     where mes = "This scenario: " +|| role ||+ scenario ||+ scenarioRole ||+ " doesn't exist"

getNodeScenario :: Cfg.FullConfig node -> L.NodeDefinitionL ()
getNodeScenario = getNode . getScenario . Cfg.nodeConfig

dispatchScenario :: LByteString -> Maybe (Cfg.FullConfig node, L.NodeDefinitionL ())
dispatchScenario (Cfg.parseConfig @S.GraphNode -> Just cfg) = Just (cfg, getNodeScenario cfg)
dispatchScenario _ = Nothing