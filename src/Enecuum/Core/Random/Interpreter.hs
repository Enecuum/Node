module Enecuum.Core.Random.Interpreter where

import           Enecuum.Prelude
import qualified Enecuum.Core.Language       as L
import qualified Control.Monad.Random.Lazy as R
import System.Random

-- | Interpret RandomL language.
interpretRandomL :: L.ERandomF a -> IO a
interpretRandomL (L.GetRandomInt k next) = do
    r <- randomRIO k
    pure $ next r
interpretRandomL (L.EvalRand r g next) = do
    let a = R.evalRand r g
    pure $ next a

runL l = foldFree interpretRandomL l

scenario :: L.ERandomL [Integer]
scenario = do
    replicateM 10 $ L.getRandomInt (5,10)

go = runL scenario
