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

import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L

import Enecuum.Framework.TestData.Nodes
import Enecuum.Framework.Testing.Runtime
import qualified Enecuum.Core.Testing.Runtime.Lens as RLens
import qualified Enecuum.Framework.Testing.Runtime.Lens as RLens

bootNodeAddr = "boot node addr"
masterNode1Addr = "master node 1 addr"


spec :: Spec
spec = describe "Master Node test" $
  it "Master Node test" $ do

    runtime <- createTestRuntime

    bootNodeRuntime   :: NodeRuntime <- createNode runtime bootNodeAddr      bootNode
    masterNodeRuntime :: NodeRuntime <- createNode runtime masterNode1Addr $ masterNode $ D.Config bootNodeAddr

    -- response <- sendRequest bootNodeRuntime HelloRequest1

    let tMsgs = runtime ^. RLens.loggerRuntime . RLens.messages
    msgs <- readTVarIO tMsgs
    msgs `shouldBe`
      [ "Serving handlersF"
      , "SendRequest conn req"
      , "OpenConnection cfg"
      , "L.WaitForSingleResponse cfg timeout"
      , "L.Multicast cfg req"
      , "Synchronize"
      , "Eval Network"
      , "Initialization"
      , "Node tag: masterNode"
      , "Serving handlersF"
      , "Initialization"
      , "Node tag: bootNode"
      ]
