{-# LANGUAGE TemplateHaskell #-}
module Enecuum.Core.CoreEffect.Language
  ( CoreEffectF (..)
  , CoreEffect
  , evalLogger
  , evalRandom
  ) where

import           Enecuum.Core.ControlFlow.Language (ControlFlow (..), ControlFlowL)
import           Enecuum.Core.FileSystem.Language
import           Enecuum.Core.Logger.Language      (Logger, LoggerL, logMessage)
import           Enecuum.Core.Random.Language
import           Enecuum.Core.ControlFlow.Language (ControlFlow (..), ControlFlowL)
import           Enecuum.Core.Database.Language (DatabaseL, Database(..))
import           Language.Haskell.TH.MakeFunctor (makeFunctorInstance)
import           Enecuum.Prelude hiding (readFile)

-- | Core effects container language.
data CoreEffectF next where
  -- | Logger effect
  EvalLogger      :: LoggerL ()     -> (() -> next) -> CoreEffectF next
  -- | Random effect
  EvalRandom      :: ERandomL a     -> (a  -> next) -> CoreEffectF next
  -- | FileSystem effect
  EvalFileSystem  :: FileSystemL a  -> (a -> next) -> CoreEffectF next
  -- | ControlFlow effect
  EvalControlFlow :: ControlFlowL a -> (a  -> next) -> CoreEffectF next
  -- | Database effect
  EvalDatabase    :: DatabaseL a    -> (a  -> next) -> CoreEffectF next

makeFunctorInstance ''CoreEffectF

type CoreEffect next = Free CoreEffectF next

evalLogger :: LoggerL () -> CoreEffect ()
evalLogger logger = liftF $ EvalLogger logger id

instance Logger (Free CoreEffectF) where
  logMessage level msg = evalLogger $ logMessage level msg

evalFileSystem :: FileSystemL a -> CoreEffect a
evalFileSystem filepath = liftF $ EvalFileSystem filepath id

instance FileSystem (Free CoreEffectF) where
  readFile filepath = evalFileSystem $ readFile filepath
  getHomeDirectory = evalFileSystem $ getHomeDirectory
  createFilePath filepath = evalFileSystem $ createFilePath filepath 

evalRandom :: ERandomL a -> CoreEffect a
evalRandom g = liftF $ EvalRandom g id

instance ERandom (Free CoreEffectF) where
  getRandomInt = evalRandom . getRandomInt
  getRandomByteString = evalRandom . getRandomByteString
  evalCoreCrypto = evalRandom . evalCoreCrypto
  nextUUID = evalRandom $ nextUUID
  
evalControlFlow :: ControlFlowL a -> CoreEffect a
evalControlFlow a = liftF $ EvalControlFlow a id

instance ControlFlow (Free CoreEffectF) where
  delay i = evalControlFlow $ delay i
