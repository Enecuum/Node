module Enecuum.Tests.Scenarios.MaliciousCryptoSpec where

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

spec :: Spec
spec = describe "Test invalid signature" $ fromHUnitTest $ TestList
    [ TestLabel "Reject invalid microblock"  testInvalidMicroblock
    , TestLabel "Reject invalid transaction" testInvalidTransaction
    ]

testInvalidTransaction :: Test
testInvalidTransaction = TestCase $ withNodesManager $ \mgr -> do
    void $ startNode Nothing mgr $ A.graphNodeTransmitter A.noDBConfig
    void $ startNode Nothing mgr A.powNode
    void $ startNode Nothing mgr $ A.poaNode A.Good $ A.PoANodeConfig 42

    -- Generate and send transactions with invalid signature to graph node
    transactions <- I.runERandomL $ replicateM A.transactionsInMicroblock $ A.generateBogusSignedTransaction
    forM transactions $ \tx -> do
        answer :: Either Text A.SuccessMsg <- makeIORpcRequest A.graphNodeTransmitterRpcAddress $ A.CreateTransaction tx
        answer `shouldSatisfy` isLeft

    threadDelay $ 1000 * 5000
    -- Check transaction pending on graph node, it must to be empty
    Right txPending :: Either Text [D.Transaction] <- makeIORpcRequest A.graphNodeTransmitterRpcAddress $ A.GetTransactionPending
    txPending `shouldBe` []


testInvalidMicroblock :: Test
testInvalidMicroblock = TestCase $ withNodesManager $ \mgr -> do
    void $ startNode Nothing mgr $ A.graphNodeTransmitter A.noDBConfig
    void $ startNode Nothing mgr A.powNode
    void $ startNode Nothing mgr $ A.poaNode A.Bad $ A.PoANodeConfig 42

    -- Generate and send transactions to graph node
    transactions <- I.runERandomL $ replicateM A.transactionsInMicroblock $ A.genTransaction A.Generated
    _ :: [Either Text A.SuccessMsg] <- forM transactions $ \tx ->
        makeIORpcRequest A.graphNodeTransmitterRpcAddress $ A.CreateTransaction tx

    threadDelay $ 1000 * 5000
    -- Check transaction pending on graph node
    Right txPending :: Either Text [D.Transaction] <- makeIORpcRequest A.graphNodeTransmitterRpcAddress $ A.GetTransactionPending
    (sort txPending) `shouldBe` (sort transactions)

    -- Ask pow node to generate n kblocks
    let timeGap = 0
    let kblockCount = 1
    _ :: Either Text A.SuccessMsg <- makeIORpcRequest A.powNodeRpcAddress $ A.NBlockPacketGeneration kblockCount timeGap

    threadDelay $ 1000 * 1000
    -- Check kblock pending
    Right kblocks :: Either Text D.KBlockPending <- makeIORpcRequest A.graphNodeTransmitterRpcAddress $ A.GetKBlockPending
    let kblockHash = D.toHash $ (map snd $ M.toList kblocks) !! 0

    -- Microblock was rejected on graph node (received from poa)
    Left _ :: Either Text [D.Microblock] <- makeIORpcRequest A.graphNodeTransmitterRpcAddress $ A.GetMBlocksForKBlockRequest kblockHash

    threadDelay $ 1000 * 1000
    -- Check transaction pending on graph node, it must to be the same as it was
    Right txPending :: Either Text [D.Transaction] <- makeIORpcRequest A.graphNodeTransmitterRpcAddress $ A.GetTransactionPending
    (sort txPending) `shouldBe` (sort transactions)

