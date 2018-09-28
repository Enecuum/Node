
module Enecuum.Tests.Functional.StateSpec where

import Enecuum.Prelude

import           Test.Hspec

import qualified Enecuum.Language as L
import qualified Enecuum.Domain as D

import           Enecuum.Testing
import qualified Enecuum.Testing.RLens as RLens

nodeAddress :: D.Address
nodeAddress = D.Address "0.0.0.4" 1000

spec :: Spec
spec = describe "State spec" $ do

  it "Create & read var non-atomically" $ do
    loggerRuntime <- createLoggerRuntime
    res <- evaluateNode loggerRuntime nodeAddress $ do
      var <- L.scenario $ L.atomically $ L.newVar "abc"
      val <- L.scenario $ L.atomically $ L.readVar var
      pure val
    res `shouldBe` ("abc" :: String)

  it "Create & read var atomically" $ do
    loggerRuntime <- createLoggerRuntime
    res <- evaluateNode loggerRuntime nodeAddress $ do
      val <- L.scenario $ L.atomically $ L.newVar "abc" >>= L.readVar
      pure val
    res `shouldBe` ("abc" :: String)

  it "Create & write var non-atomically" $ do
    loggerRuntime <- createLoggerRuntime
    res <- evaluateNode loggerRuntime nodeAddress $ do
      var <- L.scenario $ L.atomically $ L.newVar "abc"
      _   <- L.scenario $ L.atomically $ L.writeVar var "cde"
      val <- L.scenario $ L.atomically $ L.readVar var
      pure val
    res `shouldBe` ("cde" :: String)

  it "Create & write var atomically" $ do
    loggerRuntime <- createLoggerRuntime
    res <- evaluateNode loggerRuntime nodeAddress $ do
      val <- L.scenario $ L.atomically $ do
                var <- L.newVar "abc"
                L.writeVar var "cde"
                L.readVar var
      pure val
    res `shouldBe` ("cde" :: String)

  it "Create & read 2 vars non-atomically" $ do
    loggerRuntime <- createLoggerRuntime
    res <- evaluateNode loggerRuntime nodeAddress $ do
      var1 <- L.scenario $ L.atomically $ L.newVar "abc"
      var2 <- L.scenario $ L.atomically $ L.newVar "cde"
      val1 <- L.scenario $ L.atomically $ L.readVar var1
      val2 <- L.scenario $ L.atomically $ L.readVar var2
      pure (val1, val2)
    res `shouldBe` ("abc" :: String, "cde" :: String)

  it "Create & read 2 vars atomically" $ do
    loggerRuntime <- createLoggerRuntime
    res <- evaluateNode loggerRuntime nodeAddress $ do
      vars <- L.scenario $ L.atomically $ (,) <$> L.newVar "abc" <*> L.newVar "cde"
      val1 <- L.scenario $ L.atomically $ L.readVar $ fst vars
      val2 <- L.scenario $ L.atomically $ L.readVar $ snd vars
      pure (val1, val2)
    res `shouldBe` ("abc" :: String, "cde" :: String)
