module Enecuum.Framework.Domain.Error where

import Enecuum.Prelude

-- Temporary function for handling errors.
-- Should not be used in prod (uses unsafe `error`).

withSuccess :: (Show err, Monad m) => m (Either err a) -> m a
withSuccess act = act >>= \case
    Left  err -> error $ show err
    Right a   -> pure a

eitherToText :: Show a => Either Text a -> Text
eitherToText (Left  a) = "Server error: " <> a
eitherToText (Right a) = show a

eitherToText2 :: Either Text Text -> Text
eitherToText2 (Left  a) = "Server error: " <> a
eitherToText2 (Right a) = a
