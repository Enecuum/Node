{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Tests.Scenarios.SyncNodesSpec where

import           Data.Aeson
import qualified Enecuum.Assets.Scenarios      as A
import qualified Enecuum.Blockchain.Lens       as Lens
import qualified Enecuum.Assets.Nodes.OldNodes.PoA      as Old
import qualified Enecuum.Assets.Nodes.OldNodes.GN       as Old
import qualified Enecuum.Assets.Nodes.OldNodes.PoW.PoW  as Old
import qualified Enecuum.Domain                as D
import           Enecuum.Prelude
import           Enecuum.Testing.Integrational
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit      (fromHUnitTest)
import           Test.HUnit
import           Enecuum.Tests.Wrappers

spec :: Spec
spec = slowTest $ describe "Synchronization tests" $ fromHUnitTest $ TestList
    [TestLabel "test net sync" testNodeNet]

testNodeNet :: Test
testNodeNet = TestCase . withNodesManager $ \mgr -> do
    -- assign config
    let transmiterRpcAddress       = A.getRpcAddress A.defaultGnNodeAddress
    let receiverRpcAddress         = A.getRpcAddress A.defaultGnReceiverNodeAddress
    let powRpcAddress              = A.getRpcAddress A.defaultPoWNodeAddress
    let poaRpcAddress              = A.getRpcAddress A.defaultPoANodeAddress

    let graphNodeTransmitterConfig = A.defaultNodeConfig
    let graphNodeReceiverConfig    = A.defaultNodeConfig
            { A._gnNodePorts = A.defaultGnReceiverNodePorts
            , A._rpcSynco  = Just transmiterRpcAddress
            }

    -- Start nodes
    void $ startNode Nothing mgr $ Old.graphNodeTransmitter graphNodeTransmitterConfig
    waitForNode transmiterRpcAddress

    void $ startNode Nothing mgr Old.powNode
    waitForNode powRpcAddress

    void $ startNode Nothing mgr $ Old.poaNode Old.Good Old.defaultPoANodeConfig
    waitForNode poaRpcAddress

    -- void $ startNode Nothing mgr $ A.graphNodeReceiver graphNodeReceiverConfig
    void $ startNode Nothing mgr $ Old.graphNodeTransmitter graphNodeReceiverConfig
    waitForNode receiverRpcAddress

    -- Ask pow node to generate n kblocks
    let timeGap     = 1000 * 500
    let kblockCount = 2
    _ :: Either Text A.SuccessMsg <- makeIORpcRequest powRpcAddress $ A.NBlockPacketGeneration kblockCount timeGap

    waitForBlocks 2 transmiterRpcAddress
    waitForBlocks 2 receiverRpcAddress

    threadDelay $ 1000 * 1000
    -- Check kblock synchronization
    kBlock1 :: D.KBlock <- makeRpcRequestUntilSuccess transmiterRpcAddress A.GetLastKBlock
    kBlock2 :: D.KBlock <- makeRpcRequestUntilSuccess receiverRpcAddress    A.GetLastKBlock

    kBlock1 `shouldBe` kBlock2

    -- Check ledger synchronization
    Right (A.GetMBlocksForKBlockResponse mblocksPrev1) <- makeIORpcRequest transmiterRpcAddress
        $ A.GetMBlocksForKBlockRequest (kBlock1 ^. Lens.prevHash)
    Right (A.GetMBlocksForKBlockResponse mblocksPrev2) <- makeIORpcRequest receiverRpcAddress
        $ A.GetMBlocksForKBlockRequest (kBlock2 ^. Lens.prevHash)

    eWalletBalances1 :: [Either Text A.WalletBalanceMsg] <- forM (concat $ toKeys <$> mblocksPrev1) $ \i ->
        makeIORpcRequest transmiterRpcAddress $ A.GetWalletBalance i

    eWalletBalances2 :: [Either Text A.WalletBalanceMsg] <- forM (concat $ toKeys <$> mblocksPrev2) $ \i ->
        makeIORpcRequest receiverRpcAddress    $ A.GetWalletBalance i

    rights eWalletBalances1 `shouldBe` rights eWalletBalances2
    length (rights eWalletBalances1) `shouldSatisfy` (> 0)

    -- Mblocks for the underlying kblock should be synchronized.
    length mblocksPrev1 `shouldBe` 1
    mblocksPrev1 `shouldBe` mblocksPrev2

    where
        toKeys mblocks = (D._owner :: D.Transaction -> D.PublicKey) <$> (D._transactions :: D.Microblock -> [D.Transaction]) mblocks
