module Enecuum.Tests.Scenarios.RoutingSpec where
{-
import           Test.HUnit
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit ( fromHUnitTest )
import qualified Enecuum.Assets.Nodes.NN as A
import qualified Enecuum.Assets.Nodes.BN as A

spec :: Spec
spec = describe "Routing tests" $ fromHUnitTest $ TestList
    [TestLabel "Routing" testRouting]    

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
    
    
testRouting :: Test
testRouting = TestCase $ do -- undefined --do    
    startNode Nothing A.bnNode
    waitForNode A.bnAddress
    forM [5001..5010] (\port ->
        startNode Nothing nnNode Just port
        waitForNode $ D.Address "127.0.0.1" port)
    Right msg :: Either Text Msg <- makeIORpcRequest    
-}