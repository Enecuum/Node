module Enecuum.Tests.ScenariosSpec where

import Enecuum.Prelude

import           Test.Hspec

import qualified Enecuum.Language as L
import qualified Enecuum.Domain as D

import           Enecuum.Testing
import qualified Enecuum.Testing.RLens as RLens

spec :: Spec
spec = describe "Scenarios test" $ do
  it "Dummy test" $ do

    1 `shouldBe` (1 :: Int)
