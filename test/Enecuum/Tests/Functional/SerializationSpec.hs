{-# LANGUAGE RecordWildCards #-}
module Enecuum.Tests.Functional.SerializationSpec where

import qualified Data.Serialize                   as S
import qualified Enecuum.Blockchain.Domain        as D    
import           Enecuum.Prelude
import           Test.Hspec                       (Spec, describe, shouldBe)
import           Test.Hspec.Contrib.HUnit         (fromHUnitTest)
import           Test.HUnit                       (Test (..))
import qualified Enecuum.Core.Interpreters        as I
import qualified Enecuum.Blockchain.Lens          as Lens

spec :: Spec
spec = do
    describe "Signature" $ fromHUnitTest $ TestList
        [ TestLabel "Signature: binary serialization" testSignatureBinarySerialization]

testSignatureBinarySerialization :: Test
testSignatureBinarySerialization = TestCase $ do
    tx <- replicateM 1000 $ I.runERandomL $ D.genTransaction D.On
    let signatures = map (\t -> t ^. Lens.signature) tx
    map (S.decode . S.encode) signatures `shouldBe` map Right signatures    