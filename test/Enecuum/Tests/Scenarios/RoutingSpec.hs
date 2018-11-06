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
    let ports = [5001..5010]
    let nnNodes = map (D.Address A.localhost) ports
    forM_ ports (startNode Nothing mgr . A.nnNode . Just)

    let transmitter = head nnNodes
    let receivers = tail nnNodes

    threadDelay $ 1000 * 1000
    -- forM receivers $ \receiver -> modifyIORef connMgr $ checkFree receiver A.UDPs
    I.runNodeL undefined $ forM receivers $
        (\receiver -> do
            L.notify transmitter $ A.SendMsgTo (D.toHashGeneric receiver) 10 "!! msg !!")

    threadDelay $ 1000 * 5000
    -- forM receivers $ \receiver -> modifyIORef connMgr $ checkFree receiver A.RPC
    res <- forM receivers $ \(D.Address host rPort) -> do
        msg :: Either Text [Text] <- do
            let address = D.Address host (rPort - 1000)
            -- let address = D.Address host rPort
            makeIORpcRequest address A.GetRoutingMessages
        pure (msg, rPort)
    forM_ res (\(msg, rPort) -> do
        let Right mess = msg
        length mess `shouldSatisfy` ( >= 1))

    -- forM_ res print
    True `shouldBe` True
