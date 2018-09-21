module Enecuum.Framework.Node.Interpreter where

import Enecuum.Prelude
import Control.Monad.Free

import qualified Enecuum.Framework.Node.Language          as L
import           Enecuum.Framework.Networking.Interpreter (runNetworkingL)
import           Enecuum.Framework.Node.Runtime           (NodeRuntime)
import           Enecuum.Core.Interpreters                (runHGraphLIO, runCoreEffect)
import qualified Enecuum.Framework.RLens                  as RLens


-- | Interpret NodeL.
interpretNodeL :: NodeRuntime -> L.NodeF a -> IO a
interpretNodeL nodeRt (L.EvalStateAtomically statefulAction next) =
    error "L.EvalStateAtomically not implemented."

interpretNodeL nodeRt (L.EvalGraphIO graphModel next) =
    next <$> runHGraphLIO (nodeRt ^. RLens.graph) graphModel

interpretNodeL _ (L.EvalNetworking networking next) =
    next <$> runNetworkingL networking

interpretNodeL nodeRt (L.EvalCoreEffectNodeF coreEffects next) =
    next <$> runCoreEffect (nodeRt ^. RLens.coreRuntime) coreEffects

-- | Runs node language. Runs interpreters for the underlying languages.
runNodeL :: NodeRuntime -> L.NodeL a -> IO a
runNodeL nodeRt = foldFree (interpretNodeL nodeRt)
