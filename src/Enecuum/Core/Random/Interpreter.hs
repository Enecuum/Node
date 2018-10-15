{-# LANGUAGE PackageImports             #-}
module Enecuum.Core.Random.Interpreter where

import           Enecuum.Prelude
import qualified Enecuum.Core.Language     as L
import qualified Control.Monad.Random.Lazy as R
import           System.Random hiding (next)
import           "cryptonite" Crypto.Random (MonadRandom)
import Enecuum.Blockchain.Domain.Crypto (generateNewRandomAnonymousKeyPair, sign)
import System.Entropy

-- | Interpret RandomL language.
interpretERandomL :: L.ERandomF a -> IO a
interpretERandomL (L.GetRandomInt k next) = do
    r <- randomRIO k
    pure $ next r
interpretERandomL (L.GetRandomByteString k next) = do    
    r<- getEntropy k
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

runERandomL :: L.ERandomL a -> IO a
runERandomL = foldFree interpretERandomL






