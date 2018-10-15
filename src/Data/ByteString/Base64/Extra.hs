module Data.ByteString.Base64.Extra where
import qualified Data.ByteString.Base64                as B
import qualified Data.Text.Encoding                    as T (decodeUtf8, encodeUtf8)
import           Enecuum.Prelude 

base64ToText :: ByteString -> Text
base64ToText = T.decodeUtf8 . B.encode


textToBase64 :: (MonadPlus m) => Text -> m ByteString
textToBase64 aStr = case B.decode . T.encodeUtf8 $ aStr of
    Right a -> pure a
    Left  _ -> mzero    