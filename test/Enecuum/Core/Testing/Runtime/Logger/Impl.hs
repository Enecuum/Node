module Enecuum.Core.Testing.Runtime.Logger.Impl where

import Enecuum.Prelude

import           Eff (Eff, Member, handleRelay, runM, send, raise, replaceRelay)

import qualified Enecuum.Core.Types                         as D
import qualified Enecuum.Core.Language                      as L
import qualified Enecuum.Core.Testing.Runtime.Lens          as RLens
import           Enecuum.Core.Testing.Runtime.Types

interpretLoggerL
  :: LoggerRuntime
  -> L.LoggerL a
  -> Eff '[SIO, Exc SomeException] a
interpretLoggerL rt (L.LogMessage level msg) =
  safeIO $ atomically $ modifyTVar (rt ^. RLens.messages) (msg :)

runLoggerL
  :: LoggerRuntime
  -> Eff '[L.LoggerL, SIO, Exc SomeException] a
  -> Eff '[SIO, Exc SomeException] a
runLoggerL rt = handleRelay pure ( (>>=) . interpretLoggerL rt )
