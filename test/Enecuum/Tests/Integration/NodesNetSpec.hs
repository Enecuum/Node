{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}

module Enecuum.Tests.Integration.NodesNetSpec where

import           Test.HUnit
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit ( fromHUnitTest )
import           Data.Aeson

import           Enecuum.Prelude
import           Enecuum.Interpreters (runNodeDefinitionL)
import qualified Enecuum.Language   as L
import qualified Enecuum.Domain     as D
import qualified Enecuum.Runtime    as R
import qualified Data.Map           as M

import qualified Enecuum.Assets.Nodes.GraphNodeTransmitter  as A
import qualified Enecuum.Assets.Nodes.GraphNodeReceiver     as A
import qualified Enecuum.Assets.Nodes.PoW                   as A
import qualified Enecuum.Assets.Nodes.PoA                   as A
import qualified Enecuum.Assets.Nodes.Messages              as A
import qualified Enecuum.Assets.Nodes.Address               as A

spec :: Spec
spec = describe "Network tests" $ fromHUnitTest $ TestList
    [TestLabel "test net sync" testNodeNet]

createNodeRuntime :: IO R.NodeRuntime
createNodeRuntime = R.createVoidLoggerRuntime >>= R.createCoreRuntime >>= (\a -> R.createNodeRuntime a M.empty)

-- TODO: add runtime clearing
startNode :: L.NodeDefinitionL () -> IO ()
startNode nodeDefinition = void $ forkIO $ do
    nodeRt <- createNodeRuntime
    runNodeDefinitionL nodeRt nodeDefinition

makeIORpcRequest ::
    (FromJSON b, ToJSON a, Typeable a) => D.Address -> a -> IO (Either Text b)
makeIORpcRequest address msg = do
    nodeRt <- createNodeRuntime
    runNodeDefinitionL nodeRt $ L.evalNodeL $ L.makeRpcRequest address msg

testNodeNet :: Test
testNodeNet = TestCase $ do
    startNode A.graphNodeTransmitter
    threadDelay $ 1 * 1000 * 1000
    startNode A.powNode
    startNode $ A.poaNode D.Good
    threadDelay $ 1 * 1000 * 1000
    _ :: Either Text A.SuccessMsg <- makeIORpcRequest A.powNodeRpcAddress $ A.NBlockPacketGeneration 1
    threadDelay $ 3 * 1000 * 1000
    startNode A.graphNodeReceiver
    threadDelay $ 2 * 1000 * 1000
    kBlock1 :: Either Text D.KBlock             <- makeIORpcRequest A.graphNodeTransmitterRpcAddress A.GetLastKBlock
    kBlock2@(Right kblock)                      <- makeIORpcRequest A.graphNodeReceiverRpcAddress    A.GetLastKBlock
    Right (A.GetMBlocksForKBlockResponse mblocks) <- makeIORpcRequest A.graphNodeTransmitterRpcAddress $ A.GetMBlocksForKBlockRequest (D.toHash kblock)
    walletBalance1 :: [Either Text A.WalletBalanceMsg] <- forM (concat $ toKeys <$> mblocks) $ \i -> do
        makeIORpcRequest A.graphNodeTransmitterRpcAddress $ A.GetWalletBalance i
    walletBalance2 :: [Either Text A.WalletBalanceMsg] <- forM (concat $ toKeys <$> mblocks)  $ \i -> do
        makeIORpcRequest A.graphNodeReceiverRpcAddress    $ A.GetWalletBalance i
    _ :: Either Text A.SuccessMsg <- makeIORpcRequest A.graphNodeTransmitterRpcAddress A.Stop
    _ :: Either Text A.SuccessMsg <- makeIORpcRequest A.graphNodeReceiverRpcAddress    A.Stop
    _ :: Either Text A.SuccessMsg <- makeIORpcRequest A.powNodeRpcAddress              A.Stop

    shouldBe
        [ "kBlock1 == kBlock2: " <> show (kBlock1 == kBlock2)
        , "kBlock1 /= Right D.genesisKBlock: " <> show (kBlock1 /= Right D.genesisKBlock)
        , "kBlock2 /= Right D.genesisKBlock: " <> show (kBlock2 /= Right D.genesisKBlock)
        , "null (rights walletBalance1): " <> show (null (rights walletBalance1))
        , "show (rights walletBalance): " <> show (rights walletBalance1) :: Text
        ]
        [ "kBlock1 == kBlock2: True"
        , "kBlock1 /= Right D.genesisKBlock: True"
        , "kBlock2 /= Right D.genesisKBlock: True"
        , "null (rights walletBalance1): False"
        , "show (rights walletBalance): " <> show (rights walletBalance2) :: Text
        ]

toKeys mblocks = (D._owner :: D.Transaction -> D.PublicKey) <$> (D._transactions :: D.Microblock -> [D.Transaction]) mblocks
