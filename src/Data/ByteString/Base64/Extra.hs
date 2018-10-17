module Data.ByteString.Base64.Extra where
    
import qualified Data.ByteString.Base64                as B
import           Enecuum.Prelude 

base64ToText :: ByteString -> Text
base64ToText = decodeUtf8 . B.encode


textToBase64 :: (MonadPlus m) => Text -> m ByteString
textToBase64 aStr = case B.decode . encodeUtf8 $ aStr of
    Right a -> pure a
    Left  _ -> mzero    