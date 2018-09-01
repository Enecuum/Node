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
-- import qualified Enecuum.Framework.Runtime     as R

import Enecuum.Framework.TestData.Nodes
import Enecuum.Framework.Testing.Interpreters


spec :: Spec
spec = describe "Master Node test" $ it "Master Node test" $ do

  res <- runNode $ masterNode $ D.Config "boot node addr"

  "a" `shouldBe` ("b" :: String)
