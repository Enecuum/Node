
module Enecuum.Tests.Functional.RandomSpec where

import Enecuum.Prelude

import           Test.Hspec

import qualified Data.List as List

import qualified Enecuum.Language as L
import qualified Enecuum.Domain as D

import           Enecuum.Testing

spec :: Spec
spec = describe "Random spec" $

  it "Get 100 random numbers in range" $ do
    loggerRuntime <- createLoggerRuntime

    numbers <- evaluateNode loggerRuntime (D.Address "" 1) $ 
        replicateM 100 (L.getRandomInt (0, 10000))

    length numbers `shouldBe` 100
    all (\x -> x >= 0 && x <= 10000) numbers `shouldBe` True

    -- Hopefully, from 100 numbers there will be 5 distinct.
    length (List.nub numbers) `shouldSatisfy` (>= 5)