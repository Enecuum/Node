module Enecuum.Tests.Scenarios.PoASpec where

import qualified Data.Map                                     as M
import           Data.Typeable
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
import           Enecuum.Testing.Integrational
import           Enecuum.Tests.Helpers
import           Enecuum.Testing.Wrappers
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit                     (fromHUnitTest)
import           Test.HUnit

spec :: Spec
spec = unstableTest $ slowTest $ describe "PoA" $ fromHUnitTest $ TestList
    [TestLabel "Check microblock generation" testPoA]

testPoA :: Test
testPoA = TestCase $ withNodesManager $ \mgr -> do
    let transmitterRpcAddress       = A.getRpcAddress A.tstGraphNodeTransmitterAddress

    void $ startNode Nothing mgr $ Tst.tstGraphNode Tst.tstGraphNodeTransmitterConfig
    void $ startNode Nothing mgr Tst.powNode
    void $ startNode Nothing mgr $ Tst.poaNode Tst.Good Tst.tstGenPoANodeConfig

    -- Generate and send transactions to graph node
    transactions <- I.runERandomL $ replicateM A.transactionsInMicroblock $ A.genTransaction A.Generated
    _ :: [Either Text D.SuccessMsg] <- forM transactions $ \tx ->
        makeIORpcRequest transmitterRpcAddress $ D.CreateTransaction tx

    -- Check transaction pending on graph node
    txPending :: [D.Transaction] <- makeRpcRequestUntilSuccess transmitterRpcAddress D.GetTransactionPending
    (sort txPending) `shouldBe` (sort transactions)

    -- Ask pow node to generate n kblocks
    let timeGap = 0
    let kblockCount = 1
    _ :: Either Text D.SuccessMsg <- makeIORpcRequest (A.getRpcAddress A.tstGenPoWNodeAddress) $ D.NBlockPacketGeneration kblockCount timeGap

    -- Get last kblock from graph node
    kBlock :: D.KBlock <- makeRpcRequestUntilSuccess transmitterRpcAddress D.GetLastKBlock
    let kblockHash = D.toHash kBlock

    -- Microblock on graph node received from poa
    (D.GetMBlocksForKBlockResponse mblock) <- do
        let request = D.GetMBlocksForKBlockRequest kblockHash
        let predicate (D.GetMBlocksForKBlockResponse mblock) = length mblock == 1
        makeRpcRequestWithPredicate predicate transmitterRpcAddress request
    (length mblock) `shouldBe` 1

    -- Check transaction pending on graph node, it must to be empty now
    void $ do
        let predicate :: [D.Transaction] -> Bool
            predicate txPending = txPending == []
        makeRpcRequestWithPredicate predicate transmitterRpcAddress D.GetTransactionPending
