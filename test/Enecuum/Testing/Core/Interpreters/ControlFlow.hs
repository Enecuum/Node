module Enecuum.Testing.Core.Interpreters.ControlFlow where

import           Enecuum.Prelude
import qualified Enecuum.Core.ControlFlow.Language as L

interpretControlFlowF :: L.ControlFlowF a -> IO a
interpretControlFlowF (L.Delay i next) = do
    threadDelay i
    pure $ next ()

runControlFlow :: Free L.ControlFlowF a -> IO a
runControlFlow = foldFree interpretControlFlowF

