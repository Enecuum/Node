module Enecuum.Tests.Scenarios.MaliciousCryptoSpec where

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
import           Enecuum.Testing.Integrational
import           Enecuum.Tests.Helpers
import           Enecuum.Testing.Wrappers
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit                     (fromHUnitTest)
import           Test.HUnit

spec :: Spec
spec = unstableTest $ slowTest $ describe "Test invalid signature" $ fromHUnitTest $ TestList
    [ TestLabel "Reject invalid microblock"  testInvalidMicroblock
    , TestLabel "Reject invalid transaction" testInvalidTransaction
    -- , TestLabel "Accept valid microblock with invalid transactions. Reject invalid transactions to ledger", testMicroblockWithInvalidTransactions
    ]

testInvalidTransaction :: Test
testInvalidTransaction = TestCase $ withNodesManager $ \mgr -> do
    void $ startNode Nothing mgr $ Tst.tstGraphNode Tst.tstGraphNodeTransmitterConfig
    void $ startNode Nothing mgr Tst.powNode
    void $ startNode Nothing mgr $ Tst.poaNode Tst.Good Tst.tstGenPoANodeConfig
    let transmitterRpcAddress       = A.getRpcAddress A.tstGraphNodeTransmitterAddress

    waitForNode transmitterRpcAddress

    -- Generate and send transactions with invalid signature to graph node
    invalidTransactions <- I.runERandomL $ replicateM A.transactionsInMicroblock A.generateBogusSignedTransaction
    forM_ invalidTransactions $ \tx -> do
        answer :: Either Text D.SuccessMsg <- makeIORpcRequest transmitterRpcAddress $ D.CreateTransaction tx
        answer `shouldSatisfy` isLeft

    -- Generate and send transactions with valid signature to graph node
    validTransactions <- I.runERandomL $ replicateM A.transactionsInMicroblock $ A.genTransaction A.Generated
    forM_ validTransactions $ \tx -> do
        answer :: Either Text D.SuccessMsg <- makeIORpcRequest transmitterRpcAddress $ D.CreateTransaction tx
        answer `shouldSatisfy` isRight

    -- Check transaction pending on graph node, it must contain only valid transactions
    txPending :: [D.Transaction] <- makeRpcRequestUntilSuccess transmitterRpcAddress D.GetTransactionPending
    sort txPending `shouldBe` sort validTransactions


testInvalidMicroblock :: Test
testInvalidMicroblock = TestCase $ withNodesManager $ \mgr -> do
    void $ startNode Nothing mgr $ Tst.tstGraphNode $ Tst.tstGraphNodeTransmitterConfig
    void $ startNode Nothing mgr Tst.powNode
    void $ startNode Nothing mgr $ Tst.poaNode Tst.Bad Tst.tstGenPoANodeConfig
    let transmitterRpcAddress    = A.getRpcAddress A.tstGraphNodeTransmitterAddress

    -- Generate and send transactions to graph node
    transactions <- I.runERandomL $ replicateM A.transactionsInMicroblock $ A.genTransaction A.Generated
    _ :: [Either Text D.SuccessMsg] <- forM transactions $ \tx ->
        makeIORpcRequest transmitterRpcAddress $ D.CreateTransaction tx

    -- Check transaction pending on graph node
    txPending :: [D.Transaction] <- makeRpcRequestUntilSuccess transmitterRpcAddress D.GetTransactionPending
    sort txPending `shouldBe` sort transactions

    -- Ask pow node to generate n kblocks
    let timeGap = 0
    let kblockCount = 1
    _ :: Either Text D.SuccessMsg <- makeIORpcRequest (A.getRpcAddress A.tstGenPoWNodeAddress) $ D.NBlockPacketGeneration kblockCount timeGap

    -- Check kblock pending
    kblocks :: D.KBlockPending <- makeRpcRequestUntilSuccess transmitterRpcAddress D.GetKBlockPending
    let kblockHash = D.toHash $ head (map snd $ M.toList kblocks)

    -- Microblock was rejected on graph node (received from poa)
    Left _ :: Either Text [D.Microblock] <- makeIORpcRequest transmitterRpcAddress  $ D.GetMBlocksForKBlockRequest kblockHash

    -- Check transaction pending on graph node, it must to be the same as it was
    txPending :: [D.Transaction] <- makeRpcRequestUntilSuccess transmitterRpcAddress D.GetTransactionPending
    sort txPending `shouldBe` sort transactions

-- testMicroblockWithInvalidTransactions :: Test
-- testMicroblockWithInvalidTransactions = TestCase $ withNodesManager $ \mgr -> do
