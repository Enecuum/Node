module Enecuum.Tests.Scenarios.RoutingSpec where

import qualified Data.IORef                         as O
import qualified Data.Map                           as M
import qualified Enecuum.Assets.Scenarios           as A
import qualified Enecuum.Domain                     as D
import qualified Enecuum.Framework.Node.Interpreter as I
import qualified Enecuum.Language                   as L
import           Enecuum.Prelude
import           Enecuum.Testing.Integrational
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit           (fromHUnitTest)
import           Test.HUnit
import           Enecuum.Assets.Nodes.Routing.Messages

spec :: Spec
spec = describe "Routing tests" $ fromHUnitTest $ TestList
    -- []
    [TestLabel "All nodes received message" testRouting]

-- checkFree :: D.Address -> A.Protocol -> MapAddress -> MapAddress
-- checkFree address port mapAddress = case M.lookup address mapAddress of
--     Nothing       -> M.insert address port mapAddress
--     Just protocol -> error $ "Address: " +|| address ||+ " is already taken for " +|| protocol ||+ ""



testRouting :: Test
testRouting = TestCase $ withNodesManager $ \mgr -> do
    -- connMgr <- newIORef M.empty
    void $ startNode Nothing mgr A.bnNode
    let ports                 = [5001..5010]
    let receiverIds           = [D.toHashGeneric $ makeNodePorts1000 x| x <- [5002..5010]]
    let receiverRpcAddresses  = [D.Address A.localhost (makeNodePorts1000 x ^. rpcPort)| x <- [5002..5010]]
    let transmitterUdpAddress = D.Address A.localhost (makeNodePorts1000 5001 ^. udpPort)
    forM_ ports $ \port -> do
        threadDelay $ 1000 * 10
        startNode Nothing mgr . A.nnNode . Just $ port

    threadDelay $ 1000 * 1000
    -- forM receivers $ \receiver -> modifyIORef connMgr $ checkFree receiver A.UDPs
    I.runNodeL undefined $ forM receiverIds
        (\receiver ->
            L.notify transmitterUdpAddress $ SendMsgTo receiver 10 "!! msg !!")

    threadDelay $ 1000 * 5000
    -- forM receivers $ \receiver -> modifyIORef connMgr $ checkFree receiver A.RPC
    res <- forM receiverRpcAddresses $ \address ->
        makeIORpcRequest address A.GetRoutingMessages

    forM_ res (\(Right (msg :: [Text])) -> length msg `shouldSatisfy` (== 1))

    -- forM_ res print
    True `shouldBe` True
