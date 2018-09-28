module Enecuum.Tests.ScenariosSpec where

import Enecuum.Prelude

import           Test.Hspec

import qualified Enecuum.Language as L
import qualified Enecuum.Domain as D

import           Enecuum.Testing
import qualified Enecuum.Testing.RLens as RLens

import           Enecuum.Assets.Nodes.NetworkNode1 (networkNode1)
import           Enecuum.Assets.Nodes.NetworkNode2 (networkNode2)

spec :: Spec
spec = describe "Scenarios test" $ do
  it "NetworkNode1, NetworkNode2 test" $ do

    1 `shouldBe` 2
