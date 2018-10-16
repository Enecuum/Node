{-# LANGUAGE PackageImports #-}
module Data.ByteStringSpec where


import qualified Data.Aeson                   as A
import           Data.ByteString.Base64.Extra
import qualified Data.ByteString.Char8        as BS
import           Data.ByteString.Extra
import qualified Enecuum.Core.Interpreters    as I
import qualified Enecuum.Language             as L
import           Enecuum.Prelude
import           Test.Hspec                   (Spec, describe, hspec, it, shouldBe)
import           Test.Hspec.Contrib.HUnit     (fromHUnitTest)
import           Test.HUnit                   (Test (..))
import           Test.QuickCheck

instance Arbitrary ByteString where
    arbitrary = fmap BS.pack $ arbitrary
    shrink = map BS.pack . shrink . BS.unpack

spec :: Spec
spec = do
    describe "Bytestring spec test" $ fromHUnitTest $ TestList
        [ TestLabel "Verify bytestring json serialization" testRandomByteString]
    describe "Idempotent serialization Properties" $
        do it "Verify bytestring json serialization" $ property prop_JsonEncoding

testRandomByteString :: Test
testRandomByteString = TestCase $ do
    bs <- replicateM 1000 $ I.runERandomL $ L.getRandomByteString =<< L.getRandomInt (1,1000)
    map (A.decode . A.encode) bs `shouldBe` map Just bs

prop_JsonEncoding :: [ByteString] -> Bool
prop_JsonEncoding bs = (A.decode . A.encode) bs == Just bs
