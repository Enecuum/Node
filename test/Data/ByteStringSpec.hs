module Data.ByteStringSpec where


import qualified Data.Aeson                as A
import           Data.ByteString.Extra
import Data.ByteString.Base64.Extra
import qualified Enecuum.Core.Interpreters as I
import qualified Enecuum.Language          as L
import           Enecuum.Prelude
import           Test.Hspec                (Spec, describe, shouldBe)
import           Test.Hspec.Contrib.HUnit  (fromHUnitTest)
import           Test.HUnit                (Test (..))

spec :: Spec
spec = do
    describe "Bytestring spec test" $ fromHUnitTest $ TestList
        [ TestLabel "Verify bytestring serialization" testRandomByteString]

testRandomByteString :: Test
testRandomByteString = TestCase $ do
    bs <- replicateM 1000 $ I.runERandomL $ L.getRandomByteString =<< L.getRandomInt (1,1000)
    map (A.decode . A.encode) bs `shouldBe` map Just bs
