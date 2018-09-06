{-# OPTIONS_GHC -fno-warn-orphans #-}

module Eff.Extra where

import           Eff                                                            (Eff)
import           Eff.Exc                                                        (Exc)
import qualified Eff.State                                                      as S
import           Eff.State                                                      (State)
import           Eff.SafeIO                                                     (SIO)
import           Control.Monad.State.Class                                      (MonadState, get, put)
import           Control.Exception                                              (SomeException)

instance MonadState st (Eff '[State st, SIO, Exc SomeException]) where
  get = S.get
  put = S.put