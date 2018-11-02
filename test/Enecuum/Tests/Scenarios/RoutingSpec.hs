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
testRouting = TestCase $ do
    startNode Nothing A.clientNode
    startNode Nothing A.bnNode
    -- waitForNode A.bnAddress
    let ports = [5001..5010]
    forM ports (\port -> do
        startNode Nothing $ A.nnNode $ Just port
        -- waitForNode $ D.Address A.localhost port
        )
    let transmitter = D.Address A.localhost $ head ports
    let receivers = tail ports

    threadDelay $ 1000 * 1000
    I.runNodeL undefined $ forM receivers (\receiver -> L.notify transmitter $ A.SendMsgTo (D.toHashGeneric receiver) 10 "!! msg !!")
    threadDelay $ 1000 * 1000
    msg :: [Either Text [Text]] <- forM receivers (\port -> makeIORpcRequest (D.Address A.localhost port) $ A.GetRoutingMessages )
    -- print $ msgSend
    print $ msg
    stopNode A.clientAddress
    -- stopNode A.bnAddress
    -- forM ports (\port -> stopNode $ D.Address A.localhost port)
    True `shouldBe` True
