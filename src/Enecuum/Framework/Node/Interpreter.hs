module Enecuum.Framework.Node.Interpreter where

import Enecuum.Prelude
import Control.Monad.Free


import qualified Enecuum.Language                   as L

import Enecuum.Framework.Networking.Interpreter
import Enecuum.Framework.Node.Runtime
import Enecuum.Framework.Node.Language

import Enecuum.Core.HGraph.Interpreter
import Enecuum.Core.Interpreter



-- | Interpret NodeL. Does nothing ATM.
--interpretNodeL (L.Dummy) = L.logInfo "L.Dummy"
--
interpretNodeL :: NodeRuntime -> NodeF a -> IO a
interpretNodeL nr (L.EvalGraph graphModel next) = 
    next <$> runHGraphL (_graphRuntime nr) graphModel
-- | Eval networking.
interpretNodeL _ (L.EvalNetworking networking next) = 
    next <$> runNetworkingL networking
-- | Eval core effect.
interpretNodeL nr (L.EvalCoreEffectNodeF coreEffects next) =
    next <$> runCoreEffectF (_coreRuntime nr) coreEffects


-- | Runs node model. Runs interpreters for the underlying languages.
runNodeModel :: NodeRuntime -> Free NodeF a -> IO a
runNodeModel nr = foldFree (interpretNodeL nr)
