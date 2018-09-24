module Enecuum.Framework.Node.Interpreter where

import Enecuum.Prelude

import qualified Enecuum.Framework.State.Language         as L
import qualified Enecuum.Framework.Node.Language          as L
import qualified Enecuum.Framework.Networking.Interpreter as Impl
import           Enecuum.Framework.Runtime                (NodeRuntime)
import qualified Enecuum.Core.Interpreters                as Impl
import qualified Enecuum.Framework.State.Interpreter      as Impl
import qualified Enecuum.Framework.RLens                  as RLens
import Enecuum.Framework.Environment

-- | Interpret NodeL.
interpretNodeL :: NodeRuntime -> L.NodeF RealWorld a -> IO a
interpretNodeL nodeRt (L.EvalStateAtomically statefulAction next) =
    next <$> (atomically $ Impl.runStateL nodeRt statefulAction)

interpretNodeL nodeRt (L.EvalGraphIO (L.GraphAction _ ioRunner act) next) = do
  next <$> ioRunner act

interpretNodeL _ (L.EvalNetworking networking next) =
    next <$> Impl.runNetworkingL networking

interpretNodeL nodeRt (L.EvalCoreEffectNodeF coreEffects next) =
    next <$> Impl.runCoreEffect (nodeRt ^. RLens.coreRuntime) coreEffects

-- | Runs node language. Runs interpreters for the underlying languages.
runNodeL :: NodeRuntime -> L.NodeL RealWorld a -> IO a
runNodeL nodeRt = foldFree (interpretNodeL nodeRt)
