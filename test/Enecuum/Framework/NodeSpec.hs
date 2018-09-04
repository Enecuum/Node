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

bootNodeAddr = "boot node addr"
masterNode1Addr = "master node 1 addr"


spec :: Spec
spec = describe "Master Node test" $
  it "Master Node test" $ do

    runtime <- mkTestRuntime

    bootNodeRuntime   :: NodeRuntime <- createNode runtime bootNodeAddr      bootNode
    masterNodeRuntime :: NodeRuntime <- createNode runtime masterNode1Addr $ masterNode $ D.Config bootNodeAddr

    -- response <- sendRequest bootNodeRuntime HelloRequest1

    "a" `shouldBe` ("a" :: String)
