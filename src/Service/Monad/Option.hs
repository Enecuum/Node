{-# LANGUAGE DeriveFunctor #-}
-- | Модуль предоставляет монаду позволяющую работать со списками выборов как
--   с данными.
module Service.Monad.Option (opt, runOption, Options) where

import Control.Monad.Free

type Options f = Free (Option f)


data Option f next = Option (f -> Bool) (f -> IO()) next
    deriving (Functor)


opt :: (f -> Bool) -> (f -> IO()) -> Options f ()
opt p f = liftF (Option p f ())


runOption :: f -> Options f () -> IO ()
runOption a (Free (Option f ioFunc next))
    | f a       = ioFunc a
    | otherwise = runOption a next
runOption _ _   = return ()
