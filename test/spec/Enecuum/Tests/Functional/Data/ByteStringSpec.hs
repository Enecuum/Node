
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Enecuum.Tests.Functional.Data.ByteStringSpec where

import qualified Data.Aeson                   as A
import           Data.ByteString.Base64.Extra
import qualified Data.ByteString.Char8        as BS
import           Data.ByteString.Extra()
import           Enecuum.Prelude
import           Test.Hspec                   (Spec, describe, it)
import           Test.QuickCheck
import           Enecuum.Testing.Wrappers

instance Arbitrary ByteString where
    arbitrary = fmap BS.pack $ arbitrary
    shrink = map BS.pack . shrink . BS.unpack

spec :: Spec
spec = stableTest $ fastTest $
    describe "Bytestring property test" $ do
        it "Verify bytestring json serialization" $ property prop_JsonEncoding
        it "Verify bytestring Base64 serialization" $ property prop_Base64Encoding

prop_JsonEncoding :: ByteString -> Bool
prop_JsonEncoding bs = (A.decode . A.encode) bs == Just bs

prop_Base64Encoding :: ByteString -> Bool
prop_Base64Encoding bs = (textToBase64 . base64ToText) bs == bs
