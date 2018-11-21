module Enecuum.Tests.Scenarios.GraphNodeDBSpec where

import           Enecuum.Prelude
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit                   (fromHUnitTest)
import           Test.HUnit

import qualified Enecuum.Blockchain.Lens                    as Lens
import qualified Enecuum.Domain                             as D

import qualified Enecuum.Assets.Nodes.Address                as A
import qualified Enecuum.Assets.Nodes.Messages               as A
import qualified Enecuum.Assets.Nodes.OldNodes.PoW.PoW       as A
import qualified Enecuum.Assets.Nodes.OldNodes.GN            as A
import qualified Enecuum.Assets.Nodes.GraphNode.Config       as A
import qualified Enecuum.Assets.Nodes.PoA                   as A

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
    let blocksCount = 5
    let loggerCfg = Nothing
    let transmiterRpcAddress       = A.getRpcAddress A.defaultGnNodeAddress
    let receiverRpcAddress         = A.getRpcAddress A.defaultGnReceiverNodeAddress
    let powRpcAddress              = A.getRpcAddress A.defaultPoWNodeAddress
    let poaRpcAddress              = A.getRpcAddress A.defaultPoANodeAddress

    TestCase $ withDbAbsence dbPath $ withNodesManager $ \mgr -> do
        -- Starting nodes.
        print @Text "Starting GraphNodeTransmitter"
        transmitterNode1 <- startNode loggerCfg mgr $ A.graphNodeTransmitter cfg
        waitForNode transmiterRpcAddress

        print @Text "Starting PoW"
        powNode <- startNode loggerCfg mgr A.powNode
        waitForNode powRpcAddress

        poaNode <- startNode loggerCfg mgr (A.poaNode A.Good (error "No config"))
        waitForNode A.poaNodeRpcAddress

        print @Text "Requesting last KBlock"
        -- Checking there are none blocks.
        Right topKBlock0 :: Either Text D.KBlock <- makeIORpcRequest transmiterRpcAddress A.GetLastKBlock
        topKBlock0 ^. Lens.number `shouldBe` 0

        print @Text "Generating some blocks and checking they are generated"
        -- Generating some blocks and checking they are generated.
        _ :: Either Text A.SuccessMsg <- makeIORpcRequest (A.getRpcAddress A.defaultPoWNodeAddress) $ A.NBlockPacketGeneration blocksCount blocksDelay
        waitForBlocks blocksCount transmiterRpcAddress

        print @Text "Requesting last KBlock"
        Right topKBlock1 :: Either Text D.KBlock <- makeIORpcRequest transmiterRpcAddress A.GetLastKBlock
        topKBlock1 ^. Lens.number `shouldBe` blocksCount

        print @Text "Requesting to dump blocks."
        -- Requesting to dump blocks.
        _ :: Either Text A.SuccessMsg <-  makeIORpcRequest transmiterRpcAddress A.DumpToDB

        -- print @Text "Waiting"
        -- threadDelay $ 1000 * 1000

        print @Text "Stopping nodes"
        -- Stopping nodes, clearing graph.
        stopNode mgr transmitterNode1
        stopNode mgr powNode
        stopNode mgr poaNode

        print @Text "Starting node, checking there are no blocks."
        -- Starting node, checking there are no blocks.
        void $ startNode loggerCfg mgr $ A.graphNodeTransmitter cfg
        waitForNode transmiterRpcAddress

        Right genesisKBlock :: Either Text D.KBlock <- makeIORpcRequest transmiterRpcAddress A.GetLastKBlock
        genesisKBlock `shouldBe` D.genesisKBlock

        print @Text "Requesting to restore blocks from DB and checking they are restored."
        -- Requesting to restore blocks from DB and checking they are restored.
        _ :: Either Text A.SuccessMsg <- makeIORpcRequest transmiterRpcAddress A.RestoreFromDB
        waitForBlocks blocksCount transmiterRpcAddress

        print @Text "Requesting last KBlock"
        Right topKBlock2 :: Either Text D.KBlock <- makeIORpcRequest transmiterRpcAddress A.GetLastKBlock
        topKBlock1 `shouldBe` topKBlock2
