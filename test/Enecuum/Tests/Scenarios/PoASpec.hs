module Enecuum.Tests.Scenarios.PoASpec where

import qualified Data.Map                             as M
import qualified Enecuum.Assets.Blockchain.Generation as A
import qualified Enecuum.Assets.Scenarios             as A
import qualified Enecuum.Domain                       as D
import           Enecuum.Interpreters                 (runNodeDefinitionL)
import qualified Enecuum.Interpreters                 as I
import qualified Enecuum.Language                     as L
import           Enecuum.Prelude
import qualified Enecuum.Runtime                      as R
import           Enecuum.Testing.Integrational
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit             (fromHUnitTest)
import           Test.HUnit

spec :: Spec
spec = describe "PoA" $ fromHUnitTest $ TestList
    [TestLabel "Check microblock generation" testPoA]

testPoA :: Test
testPoA = TestCase $ withNodesManager $ \mgr -> do
    void $ startNode Nothing mgr $ A.graphNodeTransmitter A.noDBConfig
    void $ startNode Nothing mgr A.powNode
    void $ startNode Nothing mgr $ A.poaNode A.Good $ A.PoANodeConfig 42

    -- Generate and send transactions to graph node
    transactions <- I.runERandomL $ replicateM A.transactionsInMicroblock $ A.genTransaction A.Generated
    _ :: [Either Text A.SuccessMsg] <- forM transactions $ \tx ->
        makeIORpcRequest A.graphNodeTransmitterRpcAddress $ A.CreateTransaction tx

    threadDelay $ 1000 * 5000
    -- Check transaction pending on graph node
    Right txPending :: Either Text [D.Transaction] <- makeIORpcRequest A.graphNodeTransmitterRpcAddress $ A.GetTransactionPending
    (sort txPending) `shouldBe` (sort transactions)

    -- Ask pow node generate n kblock
    let timeGap = 0
    let kblockCount = 1
    _ :: Either Text A.SuccessMsg <- makeIORpcRequest A.powNodeRpcAddress $ A.NBlockPacketGeneration kblockCount timeGap

    threadDelay $ 1000 * 1000
    -- Check kblock pending
    Right kblocks :: Either Text D.KBlockPending <- makeIORpcRequest A.graphNodeTransmitterRpcAddress $ A.GetKBlockPending
    let kblockHash = D.toHash $ (map snd $ M.toList kblocks) !! 0

    -- Microblock on graph node received from poa
    _ :: Either Text [D.Microblock] <- makeIORpcRequest A.graphNodeTransmitterRpcAddress $ A.GetMBlocksForKBlockRequest kblockHash

    threadDelay $ 1000 * 1000
    -- Check transaction pending on graph node, it must to be empty now
    Right txPending :: Either Text [D.Transaction] <- makeIORpcRequest A.graphNodeTransmitterRpcAddress $ A.GetTransactionPending
    txPending `shouldBe` []
