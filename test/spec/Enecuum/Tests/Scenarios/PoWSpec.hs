module Enecuum.Tests.Scenarios.PoWSpec where

import qualified Data.Map                                     as M
import qualified Enecuum.Domain                               as D
import qualified Enecuum.Interpreters                         as I
import qualified Enecuum.Language                             as L
import           Enecuum.Prelude
import qualified Enecuum.Runtime                              as R
import qualified Enecuum.Samples.Assets.Blockchain.Generation as A
import qualified Enecuum.Samples.Assets.Nodes.Address         as A
import qualified Enecuum.Samples.Assets.Nodes.Messages        as D
import qualified Enecuum.Samples.Assets.TstScenarios          as Tst
import qualified Enecuum.Samples.Blockchain.Domain            as D
import qualified Enecuum.Samples.Blockchain.Language          as L
import qualified Enecuum.Samples.Blockchain.Lens              as Lens
import           Enecuum.Testing.Integrational
import           Enecuum.Tests.Helpers
import           Enecuum.Testing.Wrappers
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit                     (fromHUnitTest)
import           Test.HUnit

spec :: Spec
spec = unstableTest $ slowTest $ describe "PoW and graph node interaction" $ fromHUnitTest $ TestList
    [ TestLabel "Accept kblocks produced in order"        $ testAcceptKblock A.InOrder
    , TestLabel "Accept kblocks produced in random order" $ testAcceptKblock A.RandomOrder
    , TestLabel "Test kblock pending on graph node"         testKblockPending
    ]

-- Ask pow node to generate n kblocks
timeGap              = 1000
kblockCount          = 10

-- default addresses
transmitterRpcAddress = A.getRpcAddress A.tstGraphNodeTransmitterAddress
powRpcAddress        = A.getRpcAddress A.tstGenPoWNodeAddress


testAcceptKblock :: A.Ordering -> Test
testAcceptKblock order = TestCase $ withNodesManager $ \mgr -> do
    void $ startNode Nothing mgr $ Tst.tstGraphNode Tst.tstGraphNodeTransmitterConfig
    waitForNode transmitterRpcAddress
    void $ startNode Nothing mgr $ Tst.powNode' $ Tst.tstGenPoWNodeConfig { Tst._kblocksOrder = order}
    waitForNode powRpcAddress

    _ :: Either Text D.SuccessMsg <- makeIORpcRequest powRpcAddress $ D.NBlockPacketGeneration kblockCount timeGap

    -- Check that last kblock exists and it's number = kblockCount
    let predicate :: D.KBlock -> Bool
        predicate kBlock = kBlock ^. Lens.number == fromIntegral kblockCount
    void $ makeRpcRequestWithPredicate predicate transmitterRpcAddress D.GetLastKBlock

testKblockPending :: Test
testKblockPending = TestCase $ withNodesManager $ \mgr -> do
    powNode <- startNode Nothing mgr Tst.powNode

    waitForNode (A.getRpcAddress A.tstGenPoWNodeAddress)
    _ :: Either Text D.SuccessMsg <- makeIORpcRequest powRpcAddress $ D.NBlockPacketGeneration kblockCount timeGap

    -- wait until pow generate kblocks
    threadDelay $ 1000 * 1000

    void $ startNode Nothing mgr $ Tst.tstGraphNode Tst.tstGraphNodeTransmitterConfig
    -- only genesisKBlock kblock on graph node
    waitForNode transmitterRpcAddress
    Right topKBlock1 :: Either Text D.KBlock <- makeIORpcRequest transmitterRpcAddress D.GetLastKBlock
    topKBlock1 `shouldBe` D.genesisKBlock

    -- Ask pow node to generate n kblocks
    waitForNode powRpcAddress
    _ :: Either Text D.SuccessMsg <- makeIORpcRequest powRpcAddress $ D.NBlockPacketGeneration kblockCount timeGap

    -- The last generated bunch of kblocks must to be in pending on graph node
    kblocks :: D.KBlockPending <- makeRpcRequestUntilSuccess transmitterRpcAddress D.GetKBlockPending
    let kblockNumbers = map ((^. Lens.number) . snd) (M.toList kblocks)
    sort kblockNumbers `shouldBe` [kblockCount + 1 .. 2 * kblockCount]

    -- Stop pow, launch pow again, (there no data from the first pow launch now)
    stopNode mgr powNode
    void $ startNode Nothing mgr Tst.powNode

    -- Ask pow node to generate n kblocks
    waitForNode powRpcAddress
    _ :: Either Text D.SuccessMsg <- makeIORpcRequest powRpcAddress $ D.NBlockPacketGeneration kblockCount timeGap

    topKBlock2 :: D.KBlock <- do
        let predicate topKBlock = (topKBlock ^. Lens.number) == 2*kblockCount
        makeRpcRequestWithPredicate predicate transmitterRpcAddress D.GetLastKBlock

    -- Pending on graph node must be empty now
    Right kblocks :: Either Text D.KBlockPending <- makeIORpcRequest transmitterRpcAddress D.GetKBlockPending
    M.toList kblocks `shouldBe` []
