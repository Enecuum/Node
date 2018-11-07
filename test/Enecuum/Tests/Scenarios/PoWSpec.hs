module Enecuum.Tests.Scenarios.PoWSpec where

import qualified Data.Map                             as M
import qualified Enecuum.Assets.Blockchain.Generation as A
import qualified Enecuum.Assets.Scenarios             as A
import qualified Enecuum.Domain                       as D
import qualified Enecuum.Interpreters                 as I
import qualified Enecuum.Language                     as L
import           Enecuum.Prelude
import qualified Enecuum.Runtime                      as R
import           Enecuum.Testing.Integrational
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit             (fromHUnitTest)
import           Test.HUnit

spec :: Spec
spec = describe "PoW and graph node interaction" $ fromHUnitTest $ TestList
    [ TestLabel "Accept kblocks produced in order"        $ testAcceptKblock A.InOrder
    , TestLabel "Accept kblocks produced in random order" $ testAcceptKblock A.RandomOrder]

testAcceptKblock order = TestCase $ withNodesManager $ \mgr -> do
    void $ startNode Nothing mgr $ A.graphNodeTransmitter A.noDBConfig
    waitForNode A.graphNodeTransmitterRpcAddress
    void $ startNode Nothing mgr $ A.powNode' $ A.PoWNodeConfig A.defaultBlocksDelay order
    waitForNode A.powNodeRpcAddress

    -- Ask pow node to generate n kblocks
    let timeGap = 0
    let kblockCount = 1
    _ :: Either Text A.SuccessMsg <- makeIORpcRequest A.powNodeRpcAddress $ A.NBlockPacketGeneration kblockCount timeGap
    -- waitForBlocks 1 A.graphNodeTransmitterRpcAddress

    threadDelay $ 1000 * 5000
    -- Check that last kblock exists
    Right kBlock1 :: Either Text D.KBlock <- makeIORpcRequest A.graphNodeTransmitterRpcAddress A.GetLastKBlock
    print kBlock1

    -- Check kblock pending
    Right kblocks :: Either Text D.KBlockPending <- makeIORpcRequest A.graphNodeTransmitterRpcAddress $ A.GetKBlockPending
    print kblocks
    (length $ M.toList kblocks) `shouldBe` (fromIntegral kblockCount)
