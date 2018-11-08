module Enecuum.Tests.Scenarios.PoWSpec where

import qualified Data.Map                             as M
import qualified Enecuum.Assets.Blockchain.Generation as A
import qualified Enecuum.Assets.Scenarios             as A
import qualified Enecuum.Blockchain.Lens              as Lens
import qualified Enecuum.Domain                       as D
import qualified Enecuum.Interpreters                 as I
import qualified Enecuum.Language                     as L
import           Enecuum.Prelude
import qualified Enecuum.Runtime                      as R
import           Enecuum.Testing.Integrational
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit             (fromHUnitTest)
import           Test.HUnit
import           Enecuum.Tests.Wrappers

spec :: Spec
spec = slowTest $ describe "PoW and graph node interaction" $ fromHUnitTest $ TestList
    [ TestLabel "Accept kblocks produced in order"        $ testAcceptKblock A.InOrder
    , TestLabel "Accept kblocks produced in random order" $ testAcceptKblock A.RandomOrder
    , TestLabel "Test kblock pending on graph node"         testKblockPending
    ]

testAcceptKblock :: A.Ordering -> Test
testAcceptKblock order = TestCase $ withNodesManager $ \mgr -> do
    void $ startNode Nothing mgr $ A.graphNodeTransmitter A.noDBConfig
    waitForNode A.graphNodeTransmitterRpcAddress
    void $ startNode Nothing mgr $ A.powNode' $ A.PoWNodeConfig A.defaultBlocksDelay order
    waitForNode A.powNodeRpcAddress

    -- Ask pow node to generate n kblocks
    let timeGap = 0
    let kblockCount = 10
    _ :: Either Text A.SuccessMsg <- makeIORpcRequest A.powNodeRpcAddress $ A.NBlockPacketGeneration kblockCount timeGap

    threadDelay $ 1000 * 5000
    -- Check that last kblock exists and it's number = kblockCount
    Right kBlock :: Either Text D.KBlock <- makeIORpcRequest A.graphNodeTransmitterRpcAddress A.GetLastKBlock
    (kBlock ^. Lens.number)  `shouldBe` (fromIntegral kblockCount)

testKblockPending :: Test
testKblockPending = TestCase $ withNodesManager $ \mgr -> do
    powNode <- startNode Nothing mgr $ A.powNode' $ A.PoWNodeConfig A.defaultBlocksDelay A.InOrder

    threadDelay $ 1000 * 5000
    let timeGap = 0
    let kblockCount = 10
    -- Ask pow node to generate n kblocks
    _ :: Either Text A.SuccessMsg <- makeIORpcRequest A.powNodeRpcAddress $ A.NBlockPacketGeneration kblockCount timeGap

    threadDelay $ 1000 * 5000
    void $ startNode Nothing mgr $ A.graphNodeTransmitter A.noDBConfig
    threadDelay $ 1000 * 5000
    -- only genesisKBlock kblock on graph node
    Right topKBlock1 :: Either Text D.KBlock <- makeIORpcRequest A.graphNodeTransmitterRpcAddress A.GetLastKBlock
    topKBlock1 `shouldBe` D.genesisKBlock

    -- Ask pow node to generate n kblocks
    threadDelay $ 1000 * 5000
    _ :: Either Text A.SuccessMsg <- makeIORpcRequest A.powNodeRpcAddress $ A.NBlockPacketGeneration kblockCount timeGap

    threadDelay $ 1000 * 5000
    Right kBlock :: Either Text D.KBlock <- makeIORpcRequest A.graphNodeTransmitterRpcAddress A.GetLastKBlock

    -- The last generated bunch of kblocks must to be in pending on graph node
    Right kblocks :: Either Text D.KBlockPending <- makeIORpcRequest A.graphNodeTransmitterRpcAddress $ A.GetKBlockPending
    let kblockNumbers = map ((^. Lens.number) . snd) (M.toList kblocks)
    sort kblockNumbers `shouldBe` [kblockCount + 1 .. 2*kblockCount]

    stopNode mgr powNode
    threadDelay $ 1000 * 1000
    void $ startNode Nothing mgr $ A.powNode' $ A.PoWNodeConfig A.defaultBlocksDelay A.InOrder
    threadDelay $ 1000 * 1000
    -- Ask pow node to generate n kblocks
    _ :: Either Text A.SuccessMsg <- makeIORpcRequest A.powNodeRpcAddress $ A.NBlockPacketGeneration kblockCount timeGap

    threadDelay $ 1000 * 5000
    Right topKBlock2 :: Either Text D.KBlock <- makeIORpcRequest A.graphNodeTransmitterRpcAddress A.GetLastKBlock
    (topKBlock2 ^. Lens.number) `shouldBe` 2*kblockCount

    -- Pending on graph node must be empty now
    Right kblocks :: Either Text D.KBlockPending <- makeIORpcRequest A.graphNodeTransmitterRpcAddress $ A.GetKBlockPending
    (M.toList kblocks) `shouldBe` []
