module Enecuum.Tests.Scenarios.GraphNodeDBSpec where

import           Enecuum.Prelude
import           Test.HUnit
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit ( fromHUnitTest )

import qualified Enecuum.Domain                               as D
import qualified Enecuum.Blockchain.Lens                      as Lens

import qualified Enecuum.Assets.Nodes.GraphNode.Config       as A
import qualified Enecuum.Assets.Nodes.GraphNode.Transmitter  as A
import qualified Enecuum.Assets.Nodes.PoW.PoW                as A
import qualified Enecuum.Assets.Nodes.Messages               as A
import qualified Enecuum.Assets.Nodes.Address                as A

import           Enecuum.Testing.Integrational

spec :: Spec
spec = describe "Dump and restore graph test" $ fromHUnitTest $ TestList
    [TestLabel "Dump and restore graph test" dumpAndRestoreGraphTest]

dumpAndRestoreGraphTest :: Test
dumpAndRestoreGraphTest = do
    let dbOpts = D.defaultDbOptions
            { D._createIfMissing = True
            , D._errorIfExists   = False
            }
    let dbPath = "/tmp/enecuum/dumped_graph.dbm"
    let cfg = A.GraphNodeConfig
            { A._useDatabase         = True
            , A._dbModelName         = dbPath
            , A._useEnqHomeDir       = False
            , A._dbOptions           = dbOpts 
            , A._stopOnDatabaseError = True
            }

    let blocksCount = 10
    let blocksDelay = 0
    let loggerCfg = Nothing

    TestCase $ withDbAbsence dbPath $ withNodesManager $ \mgr -> do
        -- Starting nodes.
        transmitterNode1 <- startNode loggerCfg mgr $ A.graphNodeTransmitter cfg
        waitForNode A.graphNodeTransmitterRpcAddress

        powNode <- startNode loggerCfg mgr A.powNode
        waitForNode A.powNodeRpcAddress

        -- Checking there are none blocks.
        Right topKBlock0 :: Either Text D.KBlock <- makeIORpcRequest A.graphNodeTransmitterRpcAddress A.GetLastKBlock
        topKBlock0 ^. Lens.number `shouldBe` 0

        -- Generating some blocks and checking they are generated.
        _ :: Either Text A.SuccessMsg <- makeIORpcRequest A.powNodeRpcAddress $ A.NBlockPacketGeneration blocksCount blocksDelay
        waitForBlocks blocksCount A.graphNodeTransmitterRpcAddress
        
        Right topKBlock1 :: Either Text D.KBlock <- makeIORpcRequest A.graphNodeTransmitterRpcAddress A.GetLastKBlock
        topKBlock1 ^. Lens.number `shouldBe` blocksCount

        -- Requesting to dump blocks.
        _ :: Either Text A.SuccessMsg <-  makeIORpcRequest A.graphNodeTransmitterRpcAddress A.DumpToDB

        threadDelay $ 1000 * 1000

        -- Stopping nodes, clearing graph.
        stopNode mgr transmitterNode1
        stopNode mgr powNode

        -- Starting node, checking there are no blocks.
        void $ startNode loggerCfg mgr $ A.graphNodeTransmitter cfg
        waitForNode A.graphNodeTransmitterRpcAddress

        Right genesisKBlock :: Either Text D.KBlock <- makeIORpcRequest A.graphNodeTransmitterRpcAddress A.GetLastKBlock
        genesisKBlock `shouldBe` D.genesisKBlock

        -- Requesting to restore blocks from DB and checking they are restored.
        _ :: Either Text A.SuccessMsg <- makeIORpcRequest A.graphNodeTransmitterRpcAddress A.RestoreFromDB
        waitForBlocks blocksCount A.graphNodeTransmitterRpcAddress

        Right topKBlock2 :: Either Text D.KBlock <- makeIORpcRequest A.graphNodeTransmitterRpcAddress A.GetLastKBlock
        topKBlock1 `shouldBe` topKBlock2

