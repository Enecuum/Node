module Enecuum.Tests.Scenarios.RoutingSpec where

import qualified Enecuum.Assets.Scenarios      as A
import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import           Enecuum.Prelude
import           Enecuum.Testing.Integrational
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit      (fromHUnitTest)
import           Test.HUnit
import qualified Enecuum.Framework.Node.Interpreter as I

spec :: Spec
spec = describe "Routing tests" $ fromHUnitTest $ TestList
    -- []
    [TestLabel "All nodes received message" testRouting]


testRouting :: Test
testRouting = TestCase $ withNodesManager $ \mgr -> do
    void $ startNode Nothing mgr A.bnNode
    let ports = [5001..5010] -- A.testPorts 
    let nnNodes = map (D.Address A.localhost) ports
    forM_ ports (startNode Nothing mgr . A.nnNode . Just)

    let transmitter = head nnNodes
    let receivers = tail nnNodes

    threadDelay $ 1000 * 1000
    I.runNodeL undefined $ forM receivers $
        (\receiver -> L.notify transmitter $ A.SendMsgTo (D.toHashGeneric receiver) 10 "!! msg !!")
    threadDelay $ 1000 * 5000
    res <- forM receivers $ \(D.Address host rPort) -> do
        msg :: Either Text [Text] <- makeIORpcRequest ((D.Address host (rPort - 1000))) A.GetRoutingMessages
        pure (msg, rPort)
    forM_ res print       
    forM_ res (\(msg, rPort) -> do
        let Right mess = msg
        length mess `shouldSatisfy` ( >= 1))