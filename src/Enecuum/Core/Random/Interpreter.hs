module Enecuum.Core.Random.Interpreter where

import           Enecuum.Prelude
import qualified Enecuum.Core.Language     as L
import qualified Control.Monad.Random.Lazy as R
import           System.Random hiding (next)

-- | Interpret RandomL language.
interpretRandomL :: L.ERandomF a -> IO a
interpretRandomL (L.GetRandomInt k next) = do
    r <- randomRIO k
    pure $ next r
interpretRandomL (L.EvalRand r g next) = do
    let a = R.evalRand r g
    pure $ next a

runL :: L.ERandomL a -> IO a
runL = foldFree interpretRandomL

