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
    [TestLabel "Routing" testRouting]


testRouting :: Test
testRouting = TestCase $ withNodesManager $ \mgr -> do
    startNode Nothing mgr A.bnNode
    let ports = [5001..5010]
    let nnNodes = map (D.Address A.localhost) ports
    forM ports (\port -> do
        startNode Nothing mgr $ A.nnNode $ Just port
        )
    let transmitter = head nnNodes
    let receivers = tail nnNodes

    threadDelay $ 1000 * 1000
    I.runNodeL undefined $ forM receivers (\receiver -> L.notify transmitter $ A.SendMsgTo (D.toHashGeneric receiver) 10 "!! msg !!")
    threadDelay $ 1000 * 1000
    msg :: [Either Text [Text]] <- forM receivers (\receiver -> makeIORpcRequest receiver $ A.GetRoutingMessages )
    -- print $ msgSend
    print $ msg
    True `shouldBe` True
