{-# LANGUAGE PackageImports #-}
module Control.Monad.Extra (module X, tryMR, tryML, tryM, timeout) where

import                   Control.Monad
import qualified "extra" Control.Monad.Extra as X
import                   Control.Concurrent.Async (race) 
import                   Enecuum.Prelude

tryMR :: MonadCatch m => m t -> (t -> m ()) -> m ()
tryMR operation f = tryM operation (pure ()) f

tryML :: MonadCatch m => m t -> m () -> m ()
tryML operation f = tryM operation f (\_ -> pure ())

tryM :: MonadCatch m => m t -> m a -> (t -> m a) -> m a
tryM operation f g = do
    ok <- try $ operation
    case ok of
        Right res                  -> g res
        Left  (_ :: SomeException) -> f

timeout :: Int -> a -> IO b -> IO (Either a b)
timeout i res f = race (threadDelay i *> pure res) f