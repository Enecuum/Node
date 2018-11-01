module Enecuum.Tests.Scenarios.RoutingSpec where

import qualified Enecuum.Assets.Scenarios      as A
import qualified Enecuum.Domain                as D
import           Enecuum.Prelude
import           Enecuum.Testing.Integrational
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit      (fromHUnitTest)
import           Test.HUnit

spec :: Spec
spec = describe "Routing tests" $ fromHUnitTest $ TestList
    [TestLabel "Routing" testRouting]

testRouting :: Test
testRouting = TestCase $ --undefined --do
  True `shouldBe` True
--     -- startNode Nothing A.bnNode
--     -- waitForNode A.bnAddress
--     -- forM [5001..5010] (\port ->
--     --     startNode Nothing nnNode Just port
--     --     waitForNode $ D.Address "127.0.0.1" port)
--     -- Right msg :: Either Text Msg <- makeIORpcRequest
