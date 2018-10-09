module Enecuum.Framework.Domain.Error where

import Enecuum.Prelude

-- Temporary function for handling errors.
-- Should not be used in prod (uses unsafe `error`).

withSuccess :: (Show err, Monad m) => m (Either err a) -> m a
withSuccess act = act >>= \case
    Left  err -> error $ show err
    Right a   -> pure a
