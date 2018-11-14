{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Tests.Scenarios.SyncNodesSpec where

import           Data.Aeson
import qualified Enecuum.Assets.Scenarios      as A
import qualified Enecuum.Blockchain.Lens       as Lens
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
testNodeNet = TestCase $ withNodesManager $ \mgr -> do
    -- assign config
    let graphNodeTransmitterConfig = A.defaultNodeConfig {A._rpcPort = A.graphNodeTransmitterRpcPort}
    let graphNodeReceiverConfig = A.defaultNodeConfig { A._rpcPort = A.graphNodeReceiverRpcPort, A._rpcSynco = Just A.graphNodeTransmitterRpcAddress}
    let graphNodeTransmitterRpcAddress = A.graphNodeTransmitterRpcAddress
    let graphNodeReceiverRpcAddress = A.graphNodeReceiverRpcAddress

    -- Start nodes
    void $ startNode Nothing mgr $ A.graphNodeTransmitter graphNodeTransmitterConfig
    waitForNode graphNodeTransmitterRpcAddress

    void $ startNode Nothing mgr A.powNode
    waitForNode A.powNodeRpcAddress

    void $ startNode Nothing mgr $ A.poaNode A.Good A.defaultPoANodeConfig
    waitForNode A.poaNodeRpcAddress

    -- void $ startNode Nothing mgr $ A.graphNodeReceiver graphNodeReceiverConfig
    void $ startNode Nothing mgr $ A.graphNodeTransmitter graphNodeReceiverConfig
    waitForNode graphNodeReceiverRpcAddress

    -- Ask pow node to generate n kblocks
    let timeGap = (1000 * 500)
    let kblockCount = 2
    _ :: Either Text A.SuccessMsg <- makeIORpcRequest A.powNodeRpcAddress $ A.NBlockPacketGeneration kblockCount timeGap

    waitForBlocks 2 graphNodeTransmitterRpcAddress
    waitForBlocks 2 graphNodeReceiverRpcAddress

    threadDelay $ 1000 * 1000
    -- Check kblock synchronization
    kBlock1 :: D.KBlock <- makeRpcRequestUntilSuccess graphNodeTransmitterRpcAddress A.GetLastKBlock
    kBlock2 :: D.KBlock <- makeRpcRequestUntilSuccess graphNodeReceiverRpcAddress    A.GetLastKBlock

    kBlock1 `shouldBe` kBlock2

    -- Check ledger synchronization
    Right (A.GetMBlocksForKBlockResponse mblocksPrev1) <- makeIORpcRequest graphNodeTransmitterRpcAddress
        $ A.GetMBlocksForKBlockRequest (kBlock1 ^. Lens.prevHash)
    Right (A.GetMBlocksForKBlockResponse mblocksPrev2) <- makeIORpcRequest graphNodeReceiverRpcAddress
        $ A.GetMBlocksForKBlockRequest (kBlock2 ^. Lens.prevHash)

    eWalletBalances1 :: [Either Text A.WalletBalanceMsg] <- forM (concat $ toKeys <$> mblocksPrev1) $ \i ->
        makeIORpcRequest graphNodeTransmitterRpcAddress $ A.GetWalletBalance i

    eWalletBalances2 :: [Either Text A.WalletBalanceMsg] <- forM (concat $ toKeys <$> mblocksPrev2) $ \i ->
        makeIORpcRequest graphNodeReceiverRpcAddress    $ A.GetWalletBalance i

    (rights eWalletBalances1) `shouldBe` (rights eWalletBalances2)
    length (rights eWalletBalances1) `shouldSatisfy` (> 0)

    -- Mblocks for the underlying kblock should be synchronized.
    length mblocksPrev1 `shouldBe` 1
    mblocksPrev1 `shouldBe` mblocksPrev2

    where
        toKeys mblocks = (D._owner :: D.Transaction -> D.PublicKey) <$> (D._transactions :: D.Microblock -> [D.Transaction]) mblocks
