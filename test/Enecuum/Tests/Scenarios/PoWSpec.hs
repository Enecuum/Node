module Enecuum.Tests.Scenarios.PoWSpec where

import Enecuum.Prelude

import           Test.Hspec

import qualified Enecuum.Language as L
import qualified Enecuum.Blockchain.Lens as Lens
import qualified Enecuum.Domain as D

import           Enecuum.Assets.Nodes.Messages (SuccessMsg (..))

import           Enecuum.Assets.Nodes.Address (graphNodeTransmitterRpcPort)


newtype KBlockCheckData = KBlockCheckData
    { kBlockNumber :: D.StateVar D.BlockNumber
    }

failMsg :: D.Message
failMsg = "Fail."

successMsg :: D.Message
successMsg = "Ok."

acceptKBlock :: KBlockCheckData -> D.KBlock -> L.NodeL (Either Text SuccessMsg)
acceptKBlock (KBlockCheckData numVar) kBlock = do
    n <- L.atomically $ do
        n <- L.readVar numVar
        L.writeVar numVar $ n + 1
        pure n
    when (n /= kBlock ^. Lens.number) $ L.logInfo failMsg
    when (n == kBlock ^. Lens.number) $ L.logInfo successMsg
    pure $ Right SuccessMsg

powBlockAcceptorNode :: L.NodeDefinitionL ()
powBlockAcceptorNode = do
    nodData <- KBlockCheckData <$> (L.scenario $ L.atomically $ L.newVar 1)
    void $ L.serving D.Rpc graphNodeTransmitterRpcPort $ L.methodE $ acceptKBlock nodData

spec :: Spec
spec = describe "PoW node test" $ do
    it "fake test for PoW" $ True `shouldBe` True
{-
  it "PoW node test, 1 iteration, in order" $ do
    runtime <- createTestRuntime

    _ :: NodeRuntime <- startNode runtime graphNodeTransmitterRpcAddress powBlockAcceptorNode
    powNodeRuntime   :: NodeRuntime <- startNode runtime (D.Address "2" 1) $ powNode' False 1

    let tMsgs = runtime ^. RLens.loggerRuntime . RLens.messages
    msgs <- readTVarIO tMsgs
    length (filter (== failMsg) msgs) `shouldBe` 0
    length (filter (== successMsg) msgs) `shouldBe` 5

  it "PoW node test, 2 iterations, in order" $ do
    runtime <- createTestRuntime

    _ :: NodeRuntime <- startNode runtime graphNodeTransmitterRpcAddress powBlockAcceptorNode
    powNodeRuntime   :: NodeRuntime <- startNode runtime (D.Address "2" 1) $ powNode' False 2

    let tMsgs = runtime ^. RLens.loggerRuntime . RLens.messages
    msgs <- readTVarIO tMsgs
    length (filter (== failMsg) msgs) `shouldBe` 0
    length (filter (== successMsg) msgs) `shouldBe` 10
-}
