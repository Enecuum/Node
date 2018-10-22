module Enecuum.Tests.Scenarios.MaliciousCryptoSpec where

import qualified Data.Map                                  as M
import qualified Enecuum.Assets.Nodes.Address              as A
import qualified Enecuum.Assets.Nodes.Client               as A hiding (GetLastKBlock)
import qualified Enecuum.Assets.Nodes.GraphNodeReceiver    as A
import qualified Enecuum.Assets.Nodes.GraphNodeTransmitter as A
import qualified Enecuum.Assets.Nodes.Messages             as A
import qualified Enecuum.Assets.Nodes.PoA                  as A
import qualified Enecuum.Assets.Nodes.PoW                  as A
import qualified Enecuum.Domain                            as D
import           Enecuum.Interpreters                      (runNodeDefinitionL)
import qualified Enecuum.Language                          as L
import           Enecuum.Prelude
import qualified Enecuum.Runtime                           as R
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit                  (fromHUnitTest)
import           Test.HUnit

spec :: Spec
spec = describe "Malicious crypto test" $ fromHUnitTest $ TestList
    [TestLabel "Malicious PoA" testPoA]

createNodeRuntime :: IO R.NodeRuntime
createNodeRuntime = R.createVoidLoggerRuntime >>= R.createCoreRuntime >>= (\a -> R.createNodeRuntime a M.empty)

-- TODO: add runtime clearing
startNode :: L.NodeDefinitionL () -> IO ()
startNode nodeDefinition = void $ forkIO $ do
    nodeRt <- createNodeRuntime
    runNodeDefinitionL nodeRt nodeDefinition

makeIORpcRequest ::
    (FromJSON b, ToJSON a, Typeable a) => D.Address -> a -> IO (Either Text b)
makeIORpcRequest address msg = do
    nodeRt <- createNodeRuntime
    runNodeDefinitionL nodeRt $ L.evalNodeL $ L.makeRpcRequest address msg


testPoA :: Test
testPoA = TestCase $ do
    -- print "Start client"
    -- startNode A.clientNode
    -- threadDelay $ 1 * 1000 * 1000
    -- print "Start graphNodeTransmitter"
    startNode A.graphNodeTransmitter
    threadDelay $ 1 * 1000 * 1000
    _ :: Either Text A.SuccessMsg <- makeIORpcRequest A.graphNodeTransmitterRpcAddress A.Stop
    True `shouldBe` True
    -- print "Start powNode"
    -- startNode A.powNode
    -- threadDelay $ 1 * 1000 * 1000
    -- _ :: Either Text A.SuccessMsg <- makeIORpcRequest A.powNodeRpcAddress $ A.NBlockPacketGeneration 1
    -- threadDelay $ 3 * 1000 * 1000
    -- kBlocks1 :: Either Text [D.KBlock] <- makeIORpcRequest A.graphNodeTransmitterRpcAddress A.GetKBlockPending
    -- print "Start Malicious poaNode"
    -- startNode $ A.poaNode D.Bad
    -- threadDelay $ 5 * 1000 * 1000
    -- kBlocks2 :: Either Text [D.KBlock] <- makeIORpcRequest A.graphNodeTransmitterRpcAddress A.GetKBlockPending
    -- print "Start Good poaNode"
    -- startNode $ A.poaNode D.Good
    -- threadDelay $ 5 * 1000 * 1000
    -- kBlocks3 :: Either Text [D.KBlock] <- makeIORpcRequest A.graphNodeTransmitterRpcAddress A.GetKBlockPending
    -- -- _ :: Either Text A.SuccessMsg <- makeIORpcRequest A.clientNode    A.Stop
    -- _ :: Either Text A.SuccessMsg <- makeIORpcRequest A.graphNodeTransmitterRpcAddress A.Stop
    -- _ :: Either Text A.SuccessMsg <- makeIORpcRequest A.powNodeRpcAddress              A.Stop
    -- _ :: Either Text A.SuccessMsg <- makeIORpcRequest A.poaNodeAddress  A.Stop
    -- shouldBe
    --     [ "kBlocks pending is not empty: " <> show (not $ null $ rights [kBlocks1])
    --     , "kBlocks pending is still the same: " <> show (kBlocks1 == kBlocks2)
    --     , "kBlocks pending is not the same anymore: " <> show (kBlocks1 /= kBlocks3)
    --     ]
    --     [ "kBlocks pending is not empty: True"
    --     , "kBlocks pending is still the same: True"
    --     , "kBlocks pending is not the same anymore: True"
    --     ]
