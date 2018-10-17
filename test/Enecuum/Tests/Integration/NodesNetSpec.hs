{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}

module Enecuum.Tests.Integration.NodesNetSpec where

import           Test.HUnit
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit ( fromHUnitTest )

import           Enecuum.Prelude
import qualified Enecuum.Language   as L
import qualified Enecuum.Domain     as D
import qualified Enecuum.Runtime    as R

spec :: Spec
spec = describe "Network tests" $ fromHUnitTest $ TestList []

