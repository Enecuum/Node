module Data.ByteString.Extra where

import           Data.Aeson
import qualified Data.ByteString.Char8      as BS
import qualified Data.Text                  as E (Text, pack, unpack)
import           Enecuum.Prelude

instance ToJSON ByteString where
  toJSON h = String $ E.pack $ BS.unpack h

instance FromJSON ByteString where
  parseJSON (String s) = pure $ BS.pack $ E.unpack s
  -- parseJSON e          = error "ByteString: Wrong object format" ++ show e
