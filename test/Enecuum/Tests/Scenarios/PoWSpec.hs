module Enecuum.Tests.Scenarios.PoWSpec where

import Enecuum.Prelude

import           Test.Hspec

import qualified Enecuum.Language as L
import qualified Enecuum.Domain as D

import           Enecuum.Assets.Nodes.Messages (SuccessMsg (..))
import           Enecuum.Assets.Nodes.PoW (powNode')
import           Enecuum.Assets.Nodes.Address (graphNodeRpcAddress, graphNodeRpcPort)

import           Enecuum.Testing
import qualified Enecuum.Testing.RLens as RLens

acceptKBlock :: D.KBlock -> L.NodeL (Either Text SuccessMsg)
acceptKBlock kBlock = do
    L.logInfo $ "Got KBlock: " +|| kBlock ||+ "."
    pure $ Right SuccessMsg

powBlockAcceptorNode :: L.NodeDefinitionL ()
powBlockAcceptorNode = L.serving graphNodeRpcPort $ L.methodE acceptKBlock

powPack2 =
    [ "KBlock sending success."
    , "Got KBlock: KBlock {_prevHash = StringHash \"bQ3Kvm2ZmR9yECRX5mNIH0SmE0HI9RkLEJ+gEJ5CuXs=\", _number = 10, _nonce = 10, _solver = StringHash \"\\NUL\\NUL\\NUL\\NUL\\r\"}."
    , "Sending KBlock: KBlock {_prevHash = StringHash \"bQ3Kvm2ZmR9yECRX5mNIH0SmE0HI9RkLEJ+gEJ5CuXs=\", _number = 10, _nonce = 10, _solver = StringHash \"\\NUL\\NUL\\NUL\\NUL\\r\"}."
    , "KBlock sending success."
    , "Got KBlock: KBlock {_prevHash = StringHash \"Mc4sWafX0akpuxgfWx/nSc9csV4ZNzJJyI3+0CObrnE=\", _number = 9, _nonce = 9, _solver = StringHash \"\\NUL\\NUL\\NUL\\NUL\\f\"}."
    , "Sending KBlock: KBlock {_prevHash = StringHash \"Mc4sWafX0akpuxgfWx/nSc9csV4ZNzJJyI3+0CObrnE=\", _number = 9, _nonce = 9, _solver = StringHash \"\\NUL\\NUL\\NUL\\NUL\\f\"}."
    , "KBlock sending success."
    , "Got KBlock: KBlock {_prevHash = StringHash \"Lm3YtHwGHIl1ugO/wVer/1gY71eQ2bWqusWWQUec1aM=\", _number = 8, _nonce = 8, _solver = StringHash \"\\NUL\\NUL\\NUL\\NUL\\v\"}."
    , "Sending KBlock: KBlock {_prevHash = StringHash \"Lm3YtHwGHIl1ugO/wVer/1gY71eQ2bWqusWWQUec1aM=\", _number = 8, _nonce = 8, _solver = StringHash \"\\NUL\\NUL\\NUL\\NUL\\v\"}."
    , "KBlock sending success."
    , "Got KBlock: KBlock {_prevHash = StringHash \"ThopRPb0ZMCb+Xxr5kcWNLAowXeHNJ6gRtaepKUhHrg=\", _number = 7, _nonce = 7, _solver = StringHash \"\\NUL\\NUL\\NUL\\NUL\\n\"}."
    , "Sending KBlock: KBlock {_prevHash = StringHash \"ThopRPb0ZMCb+Xxr5kcWNLAowXeHNJ6gRtaepKUhHrg=\", _number = 7, _nonce = 7, _solver = StringHash \"\\NUL\\NUL\\NUL\\NUL\\n\"}."
    , "KBlock sending success."
    , "Got KBlock: KBlock {_prevHash = StringHash \"9aaLDrsKpYJ4I8f49lsCYTOcBaMbRSg4VrV8seqWJhs=\", _number = 6, _nonce = 6, _solver = StringHash \"\\NUL\\NUL\\NUL\\NUL\\t\"}."
    , "Sending KBlock: KBlock {_prevHash = StringHash \"9aaLDrsKpYJ4I8f49lsCYTOcBaMbRSg4VrV8seqWJhs=\", _number = 6, _nonce = 6, _solver = StringHash \"\\NUL\\NUL\\NUL\\NUL\\t\"}."
    , "Last hash: StringHash \"gFksm8+zRi2vnw3sb1zlwFqnRAOmsa60eZa4Y/7/bYM=\"."
    ]

powPack1 =
    [ "KBlock sending success."
    , "Got KBlock: KBlock {_prevHash = StringHash \"tCmDaDU0Glh6ph4xABHboSlmGmwdsvoJdyZVWHH0Ml0=\", _number = 5, _nonce = 5, _solver = StringHash \"\\NUL\\NUL\\NUL\\NUL\\b\"}."
    , "Sending KBlock: KBlock {_prevHash = StringHash \"tCmDaDU0Glh6ph4xABHboSlmGmwdsvoJdyZVWHH0Ml0=\", _number = 5, _nonce = 5, _solver = StringHash \"\\NUL\\NUL\\NUL\\NUL\\b\"}."
    , "KBlock sending success."
    , "Got KBlock: KBlock {_prevHash = StringHash \"wJKRTqGUhfeWlAQiLMfTDLZZytRNDQRk1gtZP/NTZ8A=\", _number = 4, _nonce = 4, _solver = StringHash \"\\NUL\\NUL\\NUL\\NUL\\a\"}."
    , "Sending KBlock: KBlock {_prevHash = StringHash \"wJKRTqGUhfeWlAQiLMfTDLZZytRNDQRk1gtZP/NTZ8A=\", _number = 4, _nonce = 4, _solver = StringHash \"\\NUL\\NUL\\NUL\\NUL\\a\"}."
    , "KBlock sending success."
    , "Got KBlock: KBlock {_prevHash = StringHash \"E2LxDxrbgthA1VVxrKOuWU036Po/uZUJBviNA8xWmRA=\", _number = 3, _nonce = 3, _solver = StringHash \"\\NUL\\NUL\\NUL\\NUL\\ACK\"}."
    , "Sending KBlock: KBlock {_prevHash = StringHash \"E2LxDxrbgthA1VVxrKOuWU036Po/uZUJBviNA8xWmRA=\", _number = 3, _nonce = 3, _solver = StringHash \"\\NUL\\NUL\\NUL\\NUL\\ACK\"}."
    , "KBlock sending success."
    , "Got KBlock: KBlock {_prevHash = StringHash \"nlD8GUhXlM90lQFwzle6++ZJXI6F+0MU25BxxptVVcs=\", _number = 2, _nonce = 2, _solver = StringHash \"\\NUL\\NUL\\NUL\\NUL\\ENQ\"}."
    , "Sending KBlock: KBlock {_prevHash = StringHash \"nlD8GUhXlM90lQFwzle6++ZJXI6F+0MU25BxxptVVcs=\", _number = 2, _nonce = 2, _solver = StringHash \"\\NUL\\NUL\\NUL\\NUL\\ENQ\"}."
    , "KBlock sending success."
    , "Got KBlock: KBlock {_prevHash = StringHash \"ESV3etAVJWFm9iEPHbdbQfnIaqTlI/gxzKr8MqtHlMs=\", _number = 1, _nonce = 1, _solver = StringHash \"\\NUL\\NUL\\NUL\\NUL\\EOT\"}."
    , "Sending KBlock: KBlock {_prevHash = StringHash \"ESV3etAVJWFm9iEPHbdbQfnIaqTlI/gxzKr8MqtHlMs=\", _number = 1, _nonce = 1, _solver = StringHash \"\\NUL\\NUL\\NUL\\NUL\\EOT\"}."
    , "Last hash: StringHash \"9aaLDrsKpYJ4I8f49lsCYTOcBaMbRSg4VrV8seqWJhs=\"."
    ]

powInit = [ "Generating Key Blocks." ]

-- TODO: check results by some other procedure, not by logs.
spec :: Spec
spec = describe "PoW node test" $ do
  it "PoW node test, 1 iteration, in order" $ do
    runtime <- createTestRuntime

    _ :: NodeRuntime <- startNode runtime graphNodeRpcAddress powBlockAcceptorNode
    powNodeRuntime   :: NodeRuntime <- startNode runtime (D.Address "2" 1) $ powNode' False 1

    let tMsgs = runtime ^. RLens.loggerRuntime . RLens.messages
    msgs <- readTVarIO tMsgs
    msgs `shouldBe` (powPack1 ++ powInit)

  it "PoW node test, 2 iterations, in order" $ do
    runtime <- createTestRuntime

    _ :: NodeRuntime <- startNode runtime graphNodeRpcAddress powBlockAcceptorNode
    powNodeRuntime   :: NodeRuntime <- startNode runtime (D.Address "2" 1) $ powNode' False 2

    let tMsgs = runtime ^. RLens.loggerRuntime . RLens.messages
    msgs <- readTVarIO tMsgs
    msgs `shouldBe` (powPack2 ++ powPack1 ++ powInit)
