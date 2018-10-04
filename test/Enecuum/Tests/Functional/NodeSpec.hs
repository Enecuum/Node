
module Enecuum.Tests.Functional.NodeSpec where

import Enecuum.Prelude

import           Test.Hspec
import           Data.Aeson as A

import           Enecuum.TestData.RPC
import           Enecuum.TestData.Nodes.Scenarios
import           Enecuum.Testing
import qualified Enecuum.Testing as T
import qualified Enecuum.Testing.RLens as RLens

import qualified Enecuum.Domain as D
import qualified Enecuum.Language as L

spec :: Spec
spec = describe "Nodes test" $ do
  it "Master node interacts with boot node" $ do

    runtime <- createTestRuntime

    bootNodeRuntime   :: NodeRuntime <- startNode runtime bootNodeAddr    bootNode
    masterNodeRuntime :: NodeRuntime <- startNode runtime masterNode1Addr masterNode

    -- TODO: restore control requests
    -- Right (D.RpcResponseResult eResponse _) <- T.sendRequest runtime bootNodeAddr
    --     $ L.makeRpcRequest (HelloRequest1 (D.formatAddress masterNode1Addr))
    -- A.fromJSON eResponse `shouldBe` (A.Success $ HelloResponse1 ("Hello, dear. " <> D.formatAddress masterNode1Addr))

    let tMsgs = runtime ^. RLens.loggerRuntime . RLens.messages
    msgs <- readTVarIO tMsgs
    msgs `shouldBe`
      [ "Master node got id: NodeID \"1\"."
      ]

  it "Network node requests data from network node" $ do

    runtime <- createTestRuntime

    void $ startNodeWithGraph runtime networkNode1Addr networkNode1
    void $ startNode runtime networkNode2Addr networkNode2

    let tMsgs = runtime ^. RLens.loggerRuntime . RLens.messages
    msgs <- readTVarIO tMsgs
    msgs `shouldBe`
      [ "balance4 (should be 111): 111."
      , "balance3 (should be Just 111): Just 111."
      , "balance2 (should be Nothing): Nothing."
      , "balance1 (should be Just 10): Just 10."
      , "balance0 (should be 0): 0."
      ]

  it "Boot node validates requests from Network node" $ do

    runtime <- createTestRuntime

    bootNodeValidationRuntime   :: NodeRuntime <- startNode runtime bootNodeAddr    bootNodeValidation
    masterNodeValidationRuntime :: NodeRuntime <- startNode runtime masterNode1Addr masterNodeValidation

    let tMsgs = runtime ^. RLens.loggerRuntime . RLens.messages
    msgs <- readTVarIO tMsgs
    msgs `shouldBe`
      [ "Master node got id: NodeID \"1\"."
      , "For the invalid request recieved ValidationResponse (Left [\"invalid\"])."
      , "For the valid request recieved ValidationResponse (Right \"correct\")."
      ]

  it "Network node uses state" $ do

    runtime <- createTestRuntime

    void $ startNodeWithGraph runtime networkNode3Addr networkNode3
    void $ startNode runtime networkNode4Addr networkNode4

    let tMsgs = runtime ^. RLens.loggerRuntime . RLens.messages
    msgs <- readTVarIO tMsgs
    msgs `shouldBe`
      [ "balance (should be 91): 91."
      ]

  it "Ping pong & Retry test" $ do

    runtime <- createTestRuntime

    void $ forkIO $ void $ startNode runtime pongServerAddress pongServingNode
    void $ startNode runtime pingClientAddress pingSendingClientNode
  
    threadDelay $ 1000 * 1000 * 2

    let tMsgs = runtime ^. RLens.loggerRuntime . RLens.messages
    msgs <- readTVarIO tMsgs
    msgs `shouldBe`
      [ "Pong handle received: Pong {pong = 3}. Sending Ping {ping = 4}."
      , "Ping handle received: Ping {ping = 3}. Sending Pong {pong = 3}."
      , "Pong handle received: Pong {pong = 2}. Sending Ping {ping = 3}."
      , "Ping handle received: Ping {ping = 2}. Sending Pong {pong = 2}."
      , "Pong handle received: Pong {pong = 1}. Sending Ping {ping = 2}."
      , "Ping handle received: Ping {ping = 1}. Sending Pong {pong = 1}."
      , "Pong handle received: Pong {pong = 0}. Sending Ping {ping = 1}."
      , "Ping handle received: Ping {ping = 0}. Sending Pong {pong = 0}."
      ]
