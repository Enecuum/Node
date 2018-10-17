{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}

module Enecuum.Tests.Integration.NodesNetSpec where

import           Test.HUnit
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit ( fromHUnitTest )

import           Enecuum.Prelude
import           Enecuum.Interpreters (runNodeDefinitionL)
import qualified Enecuum.Language   as L
import qualified Enecuum.Domain     as D
import qualified Enecuum.Runtime    as R

import qualified Enecuum.Assets.Nodes.GraphNodeTransmitter  as A
import qualified Enecuum.Assets.Nodes.GraphNodeReceiver     as A
import qualified Enecuum.Assets.Nodes.PoW                   as A
import qualified Enecuum.Assets.Nodes.PoA                   as A
import qualified Enecuum.Assets.Nodes.Messages              as A
import qualified Enecuum.Assets.Nodes.Address               as A

spec :: Spec
spec = describe "Network tests" $ fromHUnitTest $ TestList []

createNodeRuntime :: IO R.NodeRuntime
createNodeRuntime = R.createVoidLoggerRuntime >>= R.createCoreRuntime >>= R.createNodeRuntime

-- TODO: add runtime clearing
startNode :: L.NodeDefinitionL () -> IO ()
startNode nodeDefinition = void $ forkIO $ do
    nodeRt <- createNodeRuntime
    runNodeDefinitionL nodeRt nodeDefinition

makeIORpcRequest address msg = do
    nodeRt <- createNodeRuntime
    runNodeDefinitionL nodeRt $ L.evalNodeL $ L.makeRpcRequest address msg

testNodeNet :: Test
testNodeNet = TestCase $ do
    startNode A.graphNodeTransmitter
    startNode A.powNode
    startNode A.poaNode
    _ :: Either Text A.SuccessMsg <- makeIORpcRequest A.powNodeRpcAddress $ A.NBlockPacketGeneration 2
    threadDelay $ 3 * 1000 * 1000
    startNode A.graphNodeReceiver
    threadDelay $ 2 * 1000 * 1000
    -- checking of sunc between graph nodes.
