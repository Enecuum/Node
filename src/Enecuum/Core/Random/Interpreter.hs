{-# LANGUAGE PackageImports             #-}
module Enecuum.Core.Random.Interpreter where

import           Enecuum.Prelude
import qualified Enecuum.Core.Language     as L
import qualified Control.Monad.Random.Lazy as R
import           System.Random hiding (next)
import           "cryptonite" Crypto.Random (MonadRandom)
import Enecuum.Blockchain.Domain.Crypto (generateNewRandomAnonymousKeyPair, sign)

-- | Interpret RandomL language.
interpretERandomL :: L.ERandomF a -> IO a
interpretERandomL (L.GetRandomInt k next) = do
    r <- randomRIO k
    pure $ next r
interpretERandomL (L.EvalRand r g next) = do
    let a = R.evalRand r g
    pure $ next a
interpretERandomL (L.GenerateKeyPair next) = do
    kp <- generateNewRandomAnonymousKeyPair
    pure $ next kp
interpretERandomL (L.Sign key msg next) = do
    signature <- sign key msg
    pure $ next signature

-- interpretERandomL :: MonadRandom m => L.ERandomF a -> m a
-- interpretERandomL (L.GetRandomInt k next) = do
--     let g = undefined
--     r <- randomR (2,2) g
--     pure $ next $ fst r
-- interpretERandomL (L.EvalRand r g next) = do
--     let a = R.evalRand r g
--     pure $ next a
-- interpretERandomL (L.EvalMonadRandom next) = do
--     pure $ next   

runERandomL :: L.ERandomL a -> IO a
runERandomL = foldFree interpretERandomL


-- interpretNRandomL :: MonadRandom m => L.NRandomF a -> m a
-- interpretNRandomL (L.EvalMonadRandom next) = do
--     undefined

-- runNRandomL :: MonadRandom m => L.NRandomL a -> m a
-- runNRandomL = foldFree interpretNRandomL





