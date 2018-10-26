{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Testing.Integrational where
    
import           Data.Aeson
import qualified Data.Map           as M

import           Enecuum.Prelude
import           Enecuum.Interpreters                         (runNodeDefinitionL)
import qualified Enecuum.Language                             as L
import qualified Enecuum.Domain                               as D
import qualified Enecuum.Runtime                              as R
import qualified Enecuum.Framework.NodeDefinition.Interpreter as R
import qualified Enecuum.Assets.Nodes.Messages                as A

logConfig :: FilePath -> D.LoggerConfig
logConfig file = D.LoggerConfig "$prio $loggername: $msg" D.Debug file True

createNodeRuntime :: R.LoggerRuntime -> IO R.NodeRuntime
createNodeRuntime loggerRuntime = R.createCoreRuntime loggerRuntime >>= (`R.createNodeRuntime` M.empty)

evalNode :: L.NodeDefinitionL a -> IO a
evalNode nodeDefinition = do
    nodeRt <- R.createVoidLoggerRuntime >>= createNodeRuntime
    res <- runNodeDefinitionL nodeRt nodeDefinition
    R.clearNodeRuntime nodeRt
    pure res

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

stopNode :: D.Address -> IO ()
stopNode address = do
    eResult <- makeIORpcRequest address A.Stop
    case eResult of
        Left err           -> error $ "Stopping node " +|| address ||+ " failed: " +| err |+ "."
        Right A.SuccessMsg -> pure ()

makeIORpcRequest ::
    (FromJSON b, ToJSON a, Typeable a) => D.Address -> a -> IO (Either Text b)
makeIORpcRequest address msg = do
    nodeRt <- R.createVoidLoggerRuntime >>= createNodeRuntime
    runNodeDefinitionL nodeRt $ L.evalNodeL $ L.makeRpcRequest address msg

waitForBlocks :: Integer -> D.Address -> IO ()
waitForBlocks number address = go 0
    where
        go :: Integer -> IO ()
        go 50 = error "No valid results from node."
        go n = do
            threadDelay $ 1000 * 100
            A.GetChainLengthResponse count <- D.withSuccess $ makeIORpcRequest address A.GetChainLengthRequest
            when (count < number) $ go (n + 1)

waitForNode :: D.Address -> IO ()
waitForNode address = go 0
    where
        go :: Integer -> IO ()
        go 50 = error "Node is not ready."
        go n = do
            threadDelay $ 1000 * 100
            ePong :: Either Text A.Pong <- makeIORpcRequest address A.Ping
            when (isLeft ePong) $ go (n + 1)

