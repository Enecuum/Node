module Enecuum.Tests.Scenarios.PoASpec where

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

import           Data.Typeable
import           Enecuum.Tests.Wrappers

spec :: Spec
spec = slowTest $ describe "PoA" $ fromHUnitTest $ TestList
    [TestLabel "Check microblock generation" testPoA]

testPoA :: Test
testPoA = TestCase $ withNodesManager $ \mgr -> do
    void $ startNode Nothing mgr $ A.graphNodeTransmitter A.defaultNodeConfig
    void $ startNode Nothing mgr A.powNode
    void $ startNode Nothing mgr $ A.poaNode A.Good A.defaultPoANodeConfig

    -- Generate and send transactions to graph node
    transactions <- I.runERandomL $ replicateM A.transactionsInMicroblock $ A.genTransaction A.Generated
    _ :: [Either Text A.SuccessMsg] <- forM transactions $ \tx ->
        makeIORpcRequest A.graphNodeTransmitterRpcAddress $ A.CreateTransaction tx


    -- Check transaction pending on graph node
    txPending :: [D.Transaction] <- makeRpcRequestUntilSuccess A.graphNodeTransmitterRpcAddress $ A.GetTransactionPending
    (sort txPending) `shouldBe` (sort transactions)

    -- Ask pow node to generate n kblocks
    let timeGap = 0
    let kblockCount = 1
    _ :: Either Text A.SuccessMsg <- makeIORpcRequest A.powNodeRpcAddress $ A.NBlockPacketGeneration kblockCount timeGap

    -- Get last kblock from graph node
    kBlock :: D.KBlock <- makeRpcRequestUntilSuccess A.graphNodeTransmitterRpcAddress $ A.GetLastKBlock
    let kblockHash = D.toHash kBlock

    -- Microblock on graph node received from poa
    (A.GetMBlocksForKBlockResponse mblock) <- do
        let request = A.GetMBlocksForKBlockRequest kblockHash
        let address = A.graphNodeTransmitterRpcAddress
        let predicate (A.GetMBlocksForKBlockResponse mblock) = length mblock == 1
        makeRpcRequestWithPredicate predicate address request
    (length mblock) `shouldBe` 1

    -- Check transaction pending on graph node, it must to be empty now
    void $ do
        let predicate :: [D.Transaction] -> Bool
            predicate txPending = txPending == []
        makeRpcRequestWithPredicate predicate A.graphNodeTransmitterRpcAddress $ A.GetTransactionPending
