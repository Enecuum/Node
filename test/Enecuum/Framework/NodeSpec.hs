{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Enecuum.Framework.NodeSpec where

import           Test.Hspec
import           Data.Text                                ( Text )
import           Control.Concurrent                       ( threadDelay )
import           Eff                                      ( Eff
                                                          , Member
                                                          , handleRelay
                                                          , handleRelayS
                                                          )
import           Eff.Exc                                  ( Exc
                                                          , throwError
                                                          )
import qualified Eff.Exc.Pure                  as Exc
                                                          ( onFail )
import           Eff.TH                                   ( makeFreer )
import           Eff.SafeIO                               ( SIO
                                                          , safeIO
                                                          , runSafeIO
                                                          )
import           Eff.Reader.Pure                          ( Reader
                                                          , runReader
                                                          )
import qualified Eff.Internal                  as EI
import           Control.Exception                        ( SomeException
                                                          , try
                                                          )
import qualified Data.Aeson                    as A
import           Data.Aeson                               ( ToJSON
                                                          , FromJSON
                                                          )
import qualified Data.ByteString.Lazy          as BS
import           GHC.Generics                             ( Generic )
import           Control.Newtype.Generics                 ( Newtype
                                                          , O
                                                          , pack
                                                          , unpack
                                                          )

import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L

import Enecuum.Framework.TestData.Nodes
import Enecuum.Framework.Testing.Interpreters

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
