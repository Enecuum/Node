module Enecuum.Assets.NodesSpec where

import Enecuum.Prelude

import           Test.Hspec

import           Enecuum.Framework.Testing.Runtime as R
import qualified Enecuum.Core.Testing.Runtime.Lens as RLens
import qualified Enecuum.Framework.Testing.Lens    as RLens
import           Enecuum.Framework.Testing.Types
import qualified Enecuum.Language                  as L

import           Enecuum.Assets.Nodes.NetworkNode1 (networkNode1)
import           Enecuum.Assets.Nodes.NetworkNode2 (networkNode2)

spec :: Spec
spec = describe "Assets test" $ do
  it "NetworkNode1, NetworkNode2 test" $ do

    1 `shouldBe` 2
