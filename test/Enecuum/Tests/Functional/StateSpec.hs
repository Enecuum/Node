
module Enecuum.Tests.Functional.StateSpec where

import Enecuum.Prelude

import           Test.Hspec

import qualified Enecuum.Language as L
import qualified Enecuum.Domain as D

import           Enecuum.Testing

nodeAddress :: D.Address
nodeAddress = D.Address "0.0.0.4" 1000

spec :: Spec
spec = describe "State spec" $ do

    it "Create & read var non-atomically" $ do
        loggerRuntime <- createLoggerRuntime
        res <- evaluateNode loggerRuntime nodeAddress
                $ L.newVarIO "abc" >>= L.readVarIO
        res `shouldBe` ("abc" :: String)

    it "Create & read var atomically" $ do
        loggerRuntime <- createLoggerRuntime
        res <- evaluateNode loggerRuntime nodeAddress
                $ L.atomically
                $ L.newVar "abc" >>= L.readVar
        res `shouldBe` ("abc" :: String)

    it "Create & write var non-atomically" $ do
        loggerRuntime <- createLoggerRuntime
        res           <- evaluateNode loggerRuntime nodeAddress $ do
            var <- L.newVarIO "abc"
            _   <- L.writeVarIO var "cde"
            L.readVarIO var
        res `shouldBe` ("cde" :: String)

    it "Create & write var atomically" $ do
        loggerRuntime <- createLoggerRuntime
        res <- evaluateNode loggerRuntime nodeAddress
                $ L.atomically $ do
                    var <- L.newVar "abc"
                    L.writeVar var "cde"
                    L.readVar var
        res `shouldBe` ("cde" :: String)

    it "Create & read 2 vars non-atomically" $ do
        loggerRuntime <- createLoggerRuntime
        res           <- evaluateNode loggerRuntime nodeAddress $ do
            var1 <- L.newVarIO "abc"
            var2 <- L.newVarIO "cde"
            val1 <- L.readVarIO var1
            val2 <- L.readVarIO var2
            pure (val1, val2)
        res `shouldBe` ("abc" :: String, "cde" :: String)

    it "Create & read 2 vars atomically" $ do
        loggerRuntime <- createLoggerRuntime
        res           <- evaluateNode loggerRuntime nodeAddress $ do
            vars <- L.atomically $ (,) <$> L.newVar "abc" <*> L.newVar "cde"
            val1 <- L.readVarIO $ fst vars
            val2 <- L.readVarIO $ snd vars
            pure (val1, val2)
        res `shouldBe` ("abc" :: String, "cde" :: String)
