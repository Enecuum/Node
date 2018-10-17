{-# LANGUAGE TemplateHaskell #-}
module Enecuum.Core.CoreEffect.Language
  ( CoreEffectF (..)
  , CoreEffect
  , evalLogger
  , evalRandom
  -- , evalCrypto
  ) where

import           Enecuum.Prelude

import           Enecuum.Core.Logger.Language      (Logger, LoggerL, logMessage)
import           Enecuum.Core.Random.Language
-- import           Enecuum.Core.Crypto.Language
import           Enecuum.Core.ControlFlow.Language (ControlFlow (..), ControlFlowL)
import           Language.Haskell.TH.MakeFunctor

-- | Core effects container language.
data CoreEffectF next where
  -- | Logger effect
  EvalLogger      :: LoggerL ()     -> (() -> next) -> CoreEffectF next
  -- | Random effect
  EvalRandom      :: ERandomL a     -> (a  -> next) -> CoreEffectF next
  -- -- | Crypto effect
  -- EvalCrypto      :: CryptoL a     -> (a  -> next) -> CoreEffectF next
  -- | ControlFlow effect
  EvalControlFlow :: ControlFlowL a -> (a  -> next) -> CoreEffectF next

makeFunctorInstance ''CoreEffectF

type CoreEffect next = Free CoreEffectF next

evalLogger :: LoggerL () -> CoreEffect ()
evalLogger logger = liftF $ EvalLogger logger id

instance Logger (Free CoreEffectF) where
  logMessage level msg = evalLogger $ logMessage level msg

evalRandom :: ERandomL a -> CoreEffect a
evalRandom g = liftF $ EvalRandom g id

-- evalCrypto :: CryptoL a -> CoreEffect a
-- evalCrypto g = liftF $ EvalCrypto g id

evalControlFlow :: ControlFlowL a -> CoreEffect a
evalControlFlow a = liftF $ EvalControlFlow a id

instance ERandom (Free CoreEffectF) where
  getRandomInt = evalRandom . getRandomInt


-- instance Crypto (Free CoreEffectF) where
--   generateKeyPair = evalCrypto $ generateKeyPair
--   sign key msg = evalCrypto $ sign key msg

instance ControlFlow (Free CoreEffectF) where
  delay i = evalControlFlow $ delay i
