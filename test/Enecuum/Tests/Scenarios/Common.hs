module Enecuum.Tests.Scenarios.Common where

import qualified Data.Map                                     as M
import qualified Enecuum.Assets.Nodes.Messages                as A
import qualified Enecuum.Domain                               as D
import qualified Enecuum.Framework.NodeDefinition.Interpreter as R
import           Enecuum.Interpreters                         (runNodeDefinitionL)
import qualified Enecuum.Language                             as L
import           Enecuum.Prelude
import qualified Enecuum.Runtime                              as R

createNodeRuntime :: R.LoggerRuntime -> IO R.NodeRuntime
createNodeRuntime loggerRuntime = R.createCoreRuntime loggerRuntime >>= (`R.createNodeRuntime` M.empty)

-- TODO: add runtime clearing
startNode :: Maybe D.LoggerConfig -> L.NodeDefinitionL () -> IO ()
startNode Nothing nodeDefinition = void $ forkIO $ do
    nodeRt <- R.createVoidLoggerRuntime >>= createNodeRuntime
    runNodeDefinitionL nodeRt nodeDefinition
    R.clearNodeRuntime nodeRt
startNode (Just loggerCfg) nodeDefinition = void $ forkIO $ do
    nodeRt <- R.createLoggerRuntime loggerCfg >>= createNodeRuntime
    runNodeDefinitionL nodeRt nodeDefinition
    R.clearNodeRuntime nodeRt

makeIORpcRequest ::
    (FromJSON b, ToJSON a, Typeable a) => D.Address -> a -> IO (Either Text b)
makeIORpcRequest address msg = do
    nodeRt <- R.createVoidLoggerRuntime >>= createNodeRuntime
    runNodeDefinitionL nodeRt $ L.evalNodeL $ L.makeRpcRequest address msg


waitForNode :: D.Address -> IO ()
waitForNode address = go 0
    where
        go :: Integer -> IO ()
        go 50 = error "Node is not ready."
        go n = do
            threadDelay $ 1000 * 100
            ePong :: Either Text A.Pong <- makeIORpcRequest address A.Ping
            when (isLeft ePong) $ go (n + 1)
