{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Tests.Scenarios.SyncNodesSpec where

import           Data.Aeson
import qualified Enecuum.Domain                        as D
import           Enecuum.Prelude
import qualified Enecuum.Samples.Assets.Nodes.Address  as A
import qualified Enecuum.Samples.Assets.Nodes.CLens    as CLens
import qualified Enecuum.Samples.Assets.Nodes.Messages as D
import qualified Enecuum.Samples.Assets.TstScenarios   as Tst
import qualified Enecuum.Samples.Blockchain.Domain     as D
import qualified Enecuum.Samples.Blockchain.Language   as L
import qualified Enecuum.Samples.Blockchain.Lens       as Lens
import           Enecuum.Testing.Integrational
import           Enecuum.Tests.Helpers
import           Enecuum.Testing.Wrappers
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit              (fromHUnitTest)
import           Test.HUnit

spec :: Spec
spec = unstableTest $ slowTest $ describe "Synchronization tests" $ fromHUnitTest $ TestList
    [TestLabel "test net sync" testNodeNet]

testNodeNet :: Test
testNodeNet = TestCase . withNodesManager $ \mgr -> do
    -- assign config
    let transmiterRpcAddress       = A.getRpcAddress A.tstGraphNodeTransmitterAddress
    let receiverRpcAddress         = A.getRpcAddress A.tstGraphNodeReceiverAddress
    let powRpcAddress              = A.getRpcAddress A.tstGenPoWNodeAddress
    let poaRpcAddress              = A.getRpcAddress A.tstGenPoANodeAddress

    -- Start nodes
    void $ startNode Nothing mgr $ Tst.tstGraphNode Tst.tstGraphNodeTransmitterConfig
    waitForNode transmiterRpcAddress

    void $ startNode Nothing mgr Tst.powNode
    waitForNode powRpcAddress

    void $ startNode Nothing mgr $ Tst.poaNode Tst.Good Tst.tstGenPoANodeConfig
    waitForNode poaRpcAddress

    void $ startNode Nothing mgr $ Tst.tstGraphNode Tst.tstGraphNodeReceiverConfig
    waitForNode receiverRpcAddress

    -- Ask pow node to generate n kblocks
    let timeGap     = 1000 * 500
    let kblockCount = 2
    _ :: Either Text D.SuccessMsg <- makeIORpcRequest powRpcAddress $ D.NBlockPacketGeneration kblockCount timeGap

    waitForBlocks 2 transmiterRpcAddress
    waitForBlocks 2 receiverRpcAddress

    threadDelay $ 1000 * 1000
    -- Check kblock synchronization
    kBlock1 :: D.KBlock <- makeRpcRequestUntilSuccess transmiterRpcAddress D.GetLastKBlock
    kBlock2 :: D.KBlock <- makeRpcRequestUntilSuccess receiverRpcAddress   D.GetLastKBlock

    kBlock1 `shouldBe` kBlock2

    -- Check ledger synchronization
    Right (D.GetMBlocksForKBlockResponse mblocksPrev1) <- makeIORpcRequest transmiterRpcAddress
        $ D.GetMBlocksForKBlockRequest (kBlock1 ^. Lens.prevHash)
    Right (D.GetMBlocksForKBlockResponse mblocksPrev2) <- makeIORpcRequest receiverRpcAddress
        $ D.GetMBlocksForKBlockRequest (kBlock2 ^. Lens.prevHash)

    eWalletBalances1 :: [Either Text D.WalletBalanceMsg] <- forM (concat $ toKeys <$> mblocksPrev1) $ \i ->
        makeIORpcRequest transmiterRpcAddress $ D.GetWalletBalance i

    eWalletBalances2 :: [Either Text D.WalletBalanceMsg] <- forM (concat $ toKeys <$> mblocksPrev2) $ \i ->
        makeIORpcRequest receiverRpcAddress   $ D.GetWalletBalance i

    rights eWalletBalances1 `shouldBe` rights eWalletBalances2
    length (rights eWalletBalances1) `shouldSatisfy` (> 0)

    -- Mblocks for the underlying kblock should be synchronized.
    length mblocksPrev1 `shouldBe` 1
    mblocksPrev1 `shouldBe` mblocksPrev2

    where
        toKeys mblocks = (D._owner :: D.Transaction -> D.PublicKey) <$> (D._transactions :: D.Microblock -> [D.Transaction]) mblocks
