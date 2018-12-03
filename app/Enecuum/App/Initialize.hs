{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType   #-}

module App.Initialize where

import qualified Data.Map                           as M
import qualified Enecuum.Assets.Nodes.Address       as A
import           Enecuum.Assets.Nodes.ConfigParsing (parseConfig)
import           Enecuum.Assets.System.Directory    (clientStory)
import qualified Enecuum.Assets.TstScenarios        as Tst
import qualified Enecuum.Config                     as Cfg
import qualified Enecuum.Core.Lens                  as Lens
import qualified Enecuum.Domain                     as D
import           Enecuum.Interpreters               (clearNodeRuntime, runFileSystemL, runNodeDefinitionL)
import qualified Enecuum.Language                   as L
import           Enecuum.Prelude
import           Enecuum.Runtime                    (clearCoreRuntime, clearLoggerRuntime, createCoreRuntime,
                                                     createLoggerRuntime, createNodeRuntime)
import qualified Enecuum.Runtime                    as R
import           System.Random

createLoggerRuntime' :: D.LoggerConfig -> IO R.LoggerRuntime
createLoggerRuntime' loggerConfig' = do
    let logFile = loggerConfig' ^. Lens.logFilePath
    putTextLn $ "Log file: " +| logFile |+ "."
    putTextLn "Creating logger runtime..."
    createLoggerRuntime loggerConfig'

clearLoggerRuntime' :: R.LoggerRuntime -> IO ()
clearLoggerRuntime' loggerRt = do
    putTextLn "Clearing logger runtime..."
    clearLoggerRuntime loggerRt

createCoreRuntime' :: R.LoggerRuntime -> IO R.CoreRuntime
createCoreRuntime' loggerRt = do
    putTextLn "Creating core runtime..."
    createCoreRuntime loggerRt

clearCoreRuntime' :: R.CoreRuntime -> IO ()
clearCoreRuntime' coreRt = do
    putTextLn "Clearing core runtime..."
    clearCoreRuntime coreRt

createNodeRuntime' :: R.CoreRuntime -> IO R.NodeRuntime
createNodeRuntime' coreRt = do
    story <- runFileSystemL clientStory
    putTextLn "Creating node runtime..."
    createNodeRuntime coreRt (M.singleton "Client" story)

clearNodeRuntime' :: R.NodeRuntime -> IO ()
clearNodeRuntime' nodeRt = do
    putTextLn "Clearing node runtime..."
    clearNodeRuntime nodeRt

runNode :: D.LoggerConfig -> L.NodeDefinitionL () -> IO ()
runNode loggerConfig node =
    bracket (createLoggerRuntime' loggerConfig) clearLoggerRuntime' $ \loggerRt ->
    bracket (createCoreRuntime'   loggerRt)     clearCoreRuntime'   $ \coreRt   ->
    bracket (createNodeRuntime'   coreRt)       clearNodeRuntime'   $ \nodeRt   ->
    runNodeDefinitionL nodeRt node

runNode'
    :: Show node
    => Show (Cfg.NodeConfig node)
    => Show (Cfg.NodeScenario node)
    => Maybe (Cfg.Config node, L.NodeDefinitionL ())
    -> IO (Maybe ())
runNode' Nothing = pure Nothing
runNode' (Just (cfg, node)) = do
    putTextLn $
        "Starting node..." <>
        "\n    Node:     " +|| Cfg.node cfg         ||+
        "\n    Scenario: " +|| Cfg.nodeScenario cfg ||+ ""
    let loggerConfig' = Cfg.loggerConfig cfg
    runNode loggerConfig' node
    pure $ Just ()


-- TODO: FIXME: nodes list is duplicating here and in ConfigParsing.
initialize :: LByteString -> IO ()
initialize configSrc = do
    -- Try to parse config of unknown type (parseConfig failure invoke error)
    parseConfig configSrc
    -- Figure out type of node and run appropriate script

    -- Don't forget to update the list in ConfigParsing!
    let runners =
            [ runNode' $ Cfg.dispatchScenario @Tst.TestClient configSrc
            , runNode' $ Cfg.dispatchScenario @Tst.TestServer configSrc

            , runNode' $ Cfg.dispatchScenario @Tst.ClientNode configSrc

            , runNode' $ Cfg.dispatchScenario @Tst.TstGraphNode   configSrc
            , runNode' $ Cfg.dispatchScenario @Tst.TstGenPoWNode  configSrc
            , runNode' $ Cfg.dispatchScenario @Tst.TstGenPoANode  configSrc
            , runNode' $ Cfg.dispatchScenario @Tst.TstRealPoWNode configSrc
            ]
    sequence_ runners

startNodes tag act range cfg = do
    putTextLn $ "Start " <> tag <> " in range from " <> show (D.bottomBound range) <> " to " <> show (D.topBound range)
    forM_ (D.rangeToList range) $ \port -> do
        threadDelay 3000
        rand :: Int <- randomIO
        void $ forkIO $ void $ act cfg (D.toHashGeneric rand) port
