module Enecuum.Control.Monad.Extra where

import           Control.Monad
import           Enecuum.Prelude

tryMR :: MonadCatch m => m t -> (t -> m ()) -> m ()
tryMR operation f = tryM operation (pure ()) f

tryML :: MonadCatch m => m t -> m () -> m ()
tryML operation f = tryM operation f (\_ -> pure ())

tryM :: MonadCatch m => m t -> m () -> (t -> m ()) -> m ()
tryM operation f g = do
    ok <- try $ operation
    case ok of
        Right res                  -> g res
        Left  (_ :: SomeException) -> f