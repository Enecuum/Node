module Enecuum.Tests.Scenarios.PoASpec where

import qualified Data.Map                             as M
import qualified Enecuum.Assets.Blockchain.Generation as A
import qualified Enecuum.Assets.Nodes.Address         as A
import qualified Enecuum.Assets.Nodes.Messages        as D
import qualified Enecuum.Assets.TstScenarios          as Tst
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
    let transmiterRpcAddress       = A.getRpcAddress A.tstGraphNodeTransmitterAddress

    void $ startNode Nothing mgr $ Tst.tstGraphNode Tst.tstGraphNodeTransmitterConfig
    void $ startNode Nothing mgr Tst.powNode
    void $ startNode Nothing mgr $ Tst.poaNode Tst.Good Tst.tstGenPoANodeConfig

    -- Generate and send transactions to graph node
    transactions <- I.runERandomL $ replicateM A.transactionsInMicroblock $ A.genTransaction A.Generated
    _ :: [Either Text D.SuccessMsg] <- forM transactions $ \tx ->
        makeIORpcRequest transmiterRpcAddress $ D.CreateTransaction tx

    -- Check transaction pending on graph node
    txPending :: [D.Transaction] <- makeRpcRequestUntilSuccess transmiterRpcAddress D.GetTransactionPending
    (sort txPending) `shouldBe` (sort transactions)

    -- Ask pow node to generate n kblocks
    let timeGap = 0
    let kblockCount = 1
    _ :: Either Text D.SuccessMsg <- makeIORpcRequest (A.getRpcAddress A.tstGenPoWNodeAddress) $ D.NBlockPacketGeneration kblockCount timeGap

    -- Get last kblock from graph node
    kBlock :: D.KBlock <- makeRpcRequestUntilSuccess transmiterRpcAddress D.GetLastKBlock
    let kblockHash = D.toHash kBlock

    -- Microblock on graph node received from poa
    (D.GetMBlocksForKBlockResponse mblock) <- do
        let request = D.GetMBlocksForKBlockRequest kblockHash
        let predicate (D.GetMBlocksForKBlockResponse mblock) = length mblock == 1
        makeRpcRequestWithPredicate predicate transmiterRpcAddress request
    (length mblock) `shouldBe` 1

    -- Check transaction pending on graph node, it must to be empty now
    void $ do
        let predicate :: [D.Transaction] -> Bool
            predicate txPending = txPending == []
        makeRpcRequestWithPredicate predicate transmiterRpcAddress D.GetTransactionPending
