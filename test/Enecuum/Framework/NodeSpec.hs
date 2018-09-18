{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Enecuum.Framework.NodeSpec where

import Enecuum.Prelude

import           Test.Hspec

import           Enecuum.Framework.TestData.RPC
import           Enecuum.Framework.TestData.Nodes
import           Enecuum.Framework.Testing.Runtime
import           Enecuum.Framework.Testing.Types
import qualified Enecuum.Core.Testing.Runtime.Lens as RLens
import qualified Enecuum.Framework.Testing.Lens as RLens

spec :: Spec
spec = describe "Nodes test" $ do
  it "Master node interacts with boot node" $ do

    runtime <- createTestRuntime

    bootNodeRuntime   :: NodeRuntime <- startNode runtime bootNodeAddr    bootNode
    masterNodeRuntime :: NodeRuntime <- startNode runtime masterNode1Addr masterNode

    eResponse <- sendRequest runtime bootNodeAddr $ HelloRequest1 masterNode1Addr
    eResponse `shouldBe` (Right $ HelloResponse1 "Hello, dear. master node 1 addr")

    let tMsgs = runtime ^. RLens.loggerRuntime . RLens.messages
    msgs <- readTVarIO tMsgs
    msgs `shouldBe`
      [ "Serving handlersF"
      , "Master node got id: NodeID \"1\"."
      , "CloseConnection conn"
      , "SendRequest conn req"
      , "OpenConnection cfg"
      , "Eval Network"
      , "EvalNodeModel"
      , "Node tag: masterNode"
      , "Serving handlersF"
      , "EvalNodeModel"
      , "Node tag: bootNode"
      ]

  it "Network node requests data from network node" $ do

    runtime <- createTestRuntime

    networkNode1Runtime   :: NodeRuntime <- startNode runtime networkNode1Addr networkNode1
    networkNode2Runtime   :: NodeRuntime <- startNode runtime networkNode2Addr networkNode2

    let tMsgs = runtime ^. RLens.loggerRuntime . RLens.messages
    msgs <- readTVarIO tMsgs
    msgs `shouldBe`
      [ "balance4 (should be 111): 111."
      , "CloseConnection conn"
      , "L.EvalGraph"
      , "SendRequest conn req"
      , "OpenConnection cfg"
      , "balance3 (should be Just 111): Just 111."
      , "CloseConnection conn"
      , "L.EvalGraph"
      , "SendRequest conn req"
      , "OpenConnection cfg"
      , "balance2 (should be Nothing): Nothing."
      , "CloseConnection conn"
      , "L.EvalGraph"
      , "SendRequest conn req"
      , "OpenConnection cfg"
      , "balance1 (should be Just 10): Just 10."
      , "CloseConnection conn"
      , "L.EvalGraph"
      , "SendRequest conn req"
      , "OpenConnection cfg"
      , "balance0 (should be 0): 0."
      , "CloseConnection conn"
      , "L.EvalGraph"
      , "SendRequest conn req"
      , "OpenConnection cfg"
      , "EvalNodeModel"
      , "Node tag: networkNode2"
      , "Serving handlersF"
      , "L.EvalGraph"
      , "EvalNodeModel"
      , "Node tag: networkNode1"
      ]
