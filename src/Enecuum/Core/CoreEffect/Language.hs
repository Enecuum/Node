{-# LANGUAGE TemplateHaskell #-}
module Enecuum.Core.CoreEffect.Language
  ( CoreEffectF (..)
  , CoreEffectL
  , evalLogger
  , evalRandom
  , IOL(..)
  ) where

import           Enecuum.Core.ControlFlow.Language (ControlFlow (..), ControlFlowL)
import           Enecuum.Core.FileSystem.Language
import           Enecuum.Core.Logger.Language      (Logger, LoggerL, logMessage)
import           Enecuum.Core.Random.Language
import           Enecuum.Core.Time.Language        (Time (..), TimeL)
import           Enecuum.Prelude                   hiding (readFile, writeFile)
import           Language.Haskell.TH.MakeFunctor   (makeFunctorInstance)

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
  -- | Time effect
  EvalTime        :: TimeL a        -> (a  -> next) -> CoreEffectF next
  -- | Impure effect. Avoid using it in production code (it's not testable).
  EvalIO          :: IO a           -> (a  -> next) -> CoreEffectF next

makeFunctorInstance ''CoreEffectF

type CoreEffectL = Free CoreEffectF

class IOL m where
  evalIO :: IO a -> m a

instance IOL CoreEffectL where
  evalIO io = liftF $ EvalIO io id

evalLogger :: LoggerL () -> CoreEffectL ()
evalLogger logger = liftF $ EvalLogger logger id

instance Logger CoreEffectL where
  logMessage level msg = evalLogger $ logMessage level msg

evalFileSystem :: FileSystemL a -> CoreEffectL a
evalFileSystem filepath = liftF $ EvalFileSystem filepath id

instance FileSystem CoreEffectL where
  readFile filepath = evalFileSystem $ readFile filepath
  writeFile filename text = evalFileSystem $ writeFile filename text
  appendFile filename text = evalFileSystem $ writeFile filename text
  getHomeDirectory = evalFileSystem getHomeDirectory
  createFilePath filepath = evalFileSystem $ createFilePath filepath
  doesFileExist    = evalFileSystem . doesFileExist

evalRandom :: ERandomL a -> CoreEffectL a
evalRandom g = liftF $ EvalRandom g id

instance ERandom CoreEffectL where
  getRandomInt = evalRandom . getRandomInt
  getRandomByteString = evalRandom . getRandomByteString
  evalCoreCrypto = evalRandom . evalCoreCrypto
  nextUUID = evalRandom nextUUID

evalControlFlow :: ControlFlowL a -> CoreEffectL a
evalControlFlow a = liftF $ EvalControlFlow a id

instance ControlFlow CoreEffectL where
  delay i = evalControlFlow $ delay i

evalTime :: TimeL a -> CoreEffectL a
evalTime action = liftF $ EvalTime action id

instance Time CoreEffectL where
    getUTCTime   = evalTime getUTCTime
    getPosixTime = evalTime getPosixTime
