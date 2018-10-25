module Enecuum.Tests.Scenarios.PoASpec where

import qualified Data.Map                                   as M

import qualified Enecuum.Domain                             as D
import           Enecuum.Interpreters                       (runNodeDefinitionL)
import qualified Enecuum.Language                           as L
import           Enecuum.Prelude
import qualified Enecuum.Runtime                            as R

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit                   (fromHUnitTest)
import           Test.HUnit


spec :: Spec
spec = describe "PoA" $ fromHUnitTest $ TestList
    [TestLabel "transaction test" testPoA]
    
createNodeRuntime :: IO R.NodeRuntime
createNodeRuntime = R.createVoidLoggerRuntime >>= R.createCoreRuntime >>= (`R.createNodeRuntime` M.empty)

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
    -- startNode A.graphNodeTransmitter
    -- threadDelay $ 1 * 1000 * 1000
    -- _ :: Either Text A.SuccessMsg <- makeIORpcRequest A.graphNodeTransmitterRpcAddress A.Stop
    True `shouldBe` True    
    -- startNode A.clientNode
    -- threadDelay $ 1 * 1000 * 1000     
    -- startNode A.graphNodeTransmitter
    -- threadDelay $ 1 * 1000 * 1000
    -- tx <- D.genTransaction D.Off
    -- _ :: Either Text A.SuccessMsg <- makeIORpcRequest A.powNodeRpcAddress $ A.NBlockPacketGeneration 1    