module Enecuum.Core.ControlFlow.Interpreter where

import           Enecuum.Prelude
import qualified Enecuum.Core.ControlFlow.Language as L
import qualified Enecuum.Runtime as Rt

interpretControlFlowF :: Rt.CoreRuntime -> L.ControlFlowF a -> IO a
interpretControlFlowF _ (L.Delay i next) = do
    threadDelay i
    pure $ next ()

runControlFlow :: Rt.CoreRuntime -> Free L.ControlFlowF a -> IO a
runControlFlow coreRt = foldFree (interpretControlFlowF coreRt)
        