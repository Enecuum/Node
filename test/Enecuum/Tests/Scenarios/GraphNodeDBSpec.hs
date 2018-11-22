module Enecuum.Tests.Scenarios.GraphNodeDBSpec where

import           Enecuum.Prelude
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit                 (fromHUnitTest)
import           Test.HUnit

import qualified Enecuum.Blockchain.Lens                  as Lens
import qualified Enecuum.Domain                           as D

import qualified Enecuum.Assets.Blockchain.Generation     as A
import qualified Enecuum.Assets.Nodes.Address             as A
import qualified Enecuum.Assets.Nodes.GraphNode.Config    as A
import qualified Enecuum.Assets.Nodes.Messages            as A
import qualified Enecuum.Assets.Nodes.OldNodes.GN         as A
import qualified Enecuum.Assets.Nodes.OldNodes.PoA        as A
import qualified Enecuum.Assets.Nodes.OldNodes.PoW.Config as A
import qualified Enecuum.Assets.Nodes.OldNodes.PoW.PoW    as A

import           Enecuum.Testing.Integrational
import           Enecuum.Tests.Wrappers

spec :: Spec
spec = slowTest $ describe "Dump and restore graph test" $ fromHUnitTest $ TestList
    [TestLabel "Dump and restore graph test" dumpAndRestoreGraphTest]

dumpAndRestoreGraphTest :: Test
dumpAndRestoreGraphTest = do
    let dbOpts = D.defaultDbOptions
            { D._createIfMissing = True
            , D._errorIfExists   = False
            }
    let dbPath = "/tmp/enecuum/dumped_graph.dbm"
    let dbConfig = A.DBConfig
            { A._useDatabase         = True
            , A._dbModelName         = dbPath
            , A._useEnqHomeDir       = False
            , A._dbOptions           = dbOpts
            , A._stopOnDatabaseError = True
            }
    let cfg = A.defaultNodeConfig { A._dbConfig = dbConfig }
    let loggerCfg = Nothing

    let graphNodeRpcAddress        = A.getRpcAddress A.defaultGnNodeAddress
    let graphNodeUdpAddress        = A.getUdpAddress A.defaultGnNodeAddress
    let receiverRpcAddress         = A.getRpcAddress A.defaultGnReceiverNodeAddress
    let powRpcAddress              = A.getRpcAddress A.defaultPoWNodeAddress
    let poaRpcAddress              = A.getRpcAddress A.defaultPoANodeAddress

    let poaConfig = A.OldPoANodeConfig
          { A._poaRPCPort = D._port poaRpcAddress
          }

    let blocksCount = 3
    let blocksDelay = 1000 * 1000

    TestCase $ withDbAbsence dbPath $ withNodesManager $ \mgr -> do
        -- Starting nodes.
        transmitterNode1 <- startNode loggerCfg mgr $ A.graphNodeTransmitter cfg
        waitForNode graphNodeRpcAddress

        powNode <- startNode loggerCfg mgr A.powNode
        waitForNode powRpcAddress

        poaNode <- startNode loggerCfg mgr (A.poaNode A.Good poaConfig)
        waitForNode poaRpcAddress

        -- Checking there are none blocks.
        Right topKBlock0 :: Either Text D.KBlock <- makeIORpcRequest graphNodeRpcAddress A.GetLastKBlock
        topKBlock0 ^. Lens.number `shouldBe` 0

        -- Generating some blocks and checking they are generated.
        _ :: Either Text A.SuccessMsg <- makeIORpcRequest (A.getRpcAddress A.defaultPoWNodeAddress)
              $ A.NBlockPacketGeneration blocksCount blocksDelay
        waitForBlocks blocksCount graphNodeRpcAddress

        -- Waiting for mBlock for the last kBlock
        threadDelay $ 1000 * 1000

        -- Requesting last KBlock and its mblocks.
        Right topKBlock1 :: Either Text D.KBlock <- makeIORpcRequest graphNodeRpcAddress A.GetLastKBlock
        topKBlock1 ^. Lens.number `shouldBe` blocksCount

        Right (A.GetMBlocksForKBlockResponse mBlocks1) <- makeIORpcRequest graphNodeRpcAddress
            $ A.GetMBlocksForKBlockRequest $ D.toHash topKBlock1
        null mBlocks1 `shouldBe` False
        let (mBlock1 : _) = mBlocks1
        let txs1 = mBlock1 ^. Lens.transactions
        null txs1 `shouldBe` False

        -- Requesting to dump blocks.
        _ :: Either Text A.SuccessMsg <-  makeIORpcRequest graphNodeRpcAddress A.DumpToDB

        -- Waiting for dumping.
        threadDelay $ 1000 * 1000

        -- Stopping nodes, clearing graph.
        stopNode mgr transmitterNode1
        stopNode mgr powNode
        stopNode mgr poaNode

        -- Starting node, checking there are no blocks.
        void $ startNode loggerCfg mgr $ A.graphNodeTransmitter cfg
        waitForNode graphNodeRpcAddress

        Right genesisKBlock :: Either Text D.KBlock <- makeIORpcRequest graphNodeRpcAddress A.GetLastKBlock
        genesisKBlock `shouldBe` D.genesisKBlock

        -- Requesting to restore blocks from DB and checking they are restored.
        _ :: Either Text A.SuccessMsg <- makeIORpcRequest graphNodeRpcAddress A.RestoreFromDB
        waitForBlocks blocksCount graphNodeRpcAddress

        -- Requesting last KBlock and its mblocks.
        Right topKBlock2 :: Either Text D.KBlock <- makeIORpcRequest graphNodeRpcAddress A.GetLastKBlock
        topKBlock1 `shouldBe` topKBlock2

        Right (A.GetMBlocksForKBlockResponse mBlocks2) <- makeIORpcRequest graphNodeRpcAddress
            $ A.GetMBlocksForKBlockRequest $ D.toHash topKBlock2
        mBlocks1 `shouldBe` mBlocks2
