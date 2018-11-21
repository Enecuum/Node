module Enecuum.Tests.Scenarios.PoWSpec where

import qualified Data.Map                             as M
import qualified Enecuum.Assets.Blockchain.Generation as A
import qualified Enecuum.Assets.Nodes.OldNodes.PoA      as Old
import qualified Enecuum.Assets.Nodes.OldNodes.GN       as Old
import qualified Enecuum.Assets.Nodes.OldNodes.PoW.PoW  as Old
import qualified Enecuum.Assets.Nodes.OldNodes.PoW.Config  as Old
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

-- Ask pow node to generate n kblocks
timeGap              = 0
kblockCount          = 10

-- default addresses
transmiterRpcAddress = A.getRpcAddress A.defaultGnNodeAddress
powRpcAddress        = A.getRpcAddress A.defaultPoWNodeAddress


testAcceptKblock :: A.Ordering -> Test
testAcceptKblock order = TestCase $ withNodesManager $ \mgr -> do
    void $ startNode Nothing mgr $ Old.graphNodeTransmitter A.defaultNodeConfig
    waitForNode transmiterRpcAddress
    void $ startNode Nothing mgr $ Old.powNode' $ Old.defaultPoWNodeConfig { Old._kblocksOrder = order}
    waitForNode powRpcAddress

    _ :: Either Text A.SuccessMsg <- makeIORpcRequest powRpcAddress $ A.NBlockPacketGeneration kblockCount timeGap

    -- Check that last kblock exists and it's number = kblockCount
    let predicate :: D.KBlock -> Bool
        predicate kBlock = kBlock ^. Lens.number == fromIntegral kblockCount
    void $ makeRpcRequestWithPredicate predicate transmiterRpcAddress A.GetLastKBlock

testKblockPending :: Test
testKblockPending = TestCase $ withNodesManager $ \mgr -> do
    powNode <- startNode Nothing mgr Old.powNode

    waitForNode (A.getRpcAddress A.defaultPoWNodeAddress)
    _ :: Either Text A.SuccessMsg <- makeIORpcRequest powRpcAddress $ A.NBlockPacketGeneration kblockCount timeGap

    -- wait until pow generate kblocks
    threadDelay $ 1000 * 1000

    void $ startNode Nothing mgr $ Old.graphNodeTransmitter A.defaultNodeConfig
    -- only genesisKBlock kblock on graph node
    waitForNode transmiterRpcAddress
    Right topKBlock1 :: Either Text D.KBlock <- makeIORpcRequest transmiterRpcAddress A.GetLastKBlock
    topKBlock1 `shouldBe` D.genesisKBlock

    -- Ask pow node to generate n kblocks
    waitForNode powRpcAddress
    _ :: Either Text A.SuccessMsg <- makeIORpcRequest powRpcAddress $ A.NBlockPacketGeneration kblockCount timeGap

    -- The last generated bunch of kblocks must to be in pending on graph node
    kblocks :: D.KBlockPending <- makeRpcRequestUntilSuccess transmiterRpcAddress A.GetKBlockPending
    let kblockNumbers = map ((^. Lens.number) . snd) (M.toList kblocks)
    sort kblockNumbers `shouldBe` [kblockCount + 1 .. 2*kblockCount]

    -- Stop pow, launch pow again, (there no data from the first pow launch now)
    stopNode mgr powNode
    void $ startNode Nothing mgr Old.powNode

    -- Ask pow node to generate n kblocks
    waitForNode powRpcAddress
    _ :: Either Text A.SuccessMsg <- makeIORpcRequest powRpcAddress $ A.NBlockPacketGeneration kblockCount timeGap

    topKBlock2 :: D.KBlock <- do
        let predicate topKBlock = (topKBlock ^. Lens.number) == 2*kblockCount
        makeRpcRequestWithPredicate predicate transmiterRpcAddress A.GetLastKBlock

    -- Pending on graph node must be empty now
    Right kblocks :: Either Text D.KBlockPending <- makeIORpcRequest transmiterRpcAddress A.GetKBlockPending
    (M.toList kblocks) `shouldBe` []
