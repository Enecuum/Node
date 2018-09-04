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

    bootNodeControl <- runNode runtime bootNodeAddr $ bootNode
    masterNodeControl <- runNode runtime masterNode1Addr $ masterNode $ D.Config bootNodeAddr

    -- response <- sendRequest bootNodeControl HelloRequest1

    "a" `shouldBe` ("a" :: String)
