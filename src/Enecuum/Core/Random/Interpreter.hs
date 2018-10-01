module Enecuum.Core.Random.Interpreter where

import           Enecuum.Prelude
import qualified Enecuum.Core.Language       as L
import qualified Control.Monad.Random.Lazy as R

-- | Interpret RandomL language.
interpretRandomL :: L.RandomF a -> IO a
interpretRandomL (L.EvalRand r g next) = do
    let a = R.evalRand r g
    pure $ next a
