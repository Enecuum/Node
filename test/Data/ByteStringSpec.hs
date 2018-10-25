
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.ByteStringSpec where


import qualified Data.Aeson                   as A
import           Data.ByteString.Base64.Extra
import qualified Data.ByteString.Char8        as BS
import           Data.ByteString.Extra
--import qualified Enecuum.Core.Interpreters    as I
--import qualified Enecuum.Language             as L
import           Enecuum.Prelude
import           Test.Hspec                   (Spec, describe, it)
--import           Test.Hspec.Contrib.HUnit     (fromHUnitTest)
import           Test.QuickCheck
--import           Test.QuickCheck.Monadic (monadicIO, assert, run)

instance Arbitrary ByteString where
    arbitrary = fmap BS.pack $ arbitrary
    shrink = map BS.pack . shrink . BS.unpack

spec :: Spec
spec = do
    describe "Bytestring property test" $ do
        it "Verify bytestring json serialization" $ property prop_JsonEncoding
        it "Verify bytestring Base64 serialization" $ property prop_Base64Encoding

prop_JsonEncoding :: ByteString -> Bool
prop_JsonEncoding bs = (A.decode . A.encode) bs == Just bs

prop_Base64Encoding :: ByteString -> Bool
prop_Base64Encoding bs = (textToBase64 . base64ToText) bs == bs
