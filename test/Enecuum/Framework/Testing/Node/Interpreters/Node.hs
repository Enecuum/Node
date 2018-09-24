module Enecuum.Framework.Testing.Node.Interpreters.Node where

import Enecuum.Prelude

import qualified Enecuum.Language                                       as L
import           Enecuum.Core.HGraph.Interpreters.IO                    (runHGraphIO)

import qualified Enecuum.Core.Testing.Runtime.Interpreters              as Impl
import           Enecuum.Framework.Testing.Types
import qualified Enecuum.Framework.Testing.Lens                         as RLens
import qualified Enecuum.Framework.Testing.Node.Interpreters.Networking as Impl
import qualified Enecuum.Framework.Testing.Node.Interpreters.State      as Impl
import           Enecuum.Framework.Environment

-- | Interpret NodeL.
interpretNodeL :: NodeRuntime -> L.NodeF TestWorld a -> IO a

interpretNodeL nodeRt (L.EvalStateAtomically statefulAction next) = do
  next <$> (atomically $ Impl.runStateL nodeRt statefulAction)

interpretNodeL nodeRt (L.EvalGraphIO (L.GraphAction _ ioRunner act) next) = do
  next <$> ioRunner act

interpretNodeL nodeRt (L.EvalNetworking networkingAction next) =
  next <$> Impl.runNetworkingL nodeRt networkingAction

interpretNodeL nodeRt (L.EvalCoreEffectNodeF coreEffect next) =
  next <$> Impl.runCoreEffect (nodeRt ^. RLens.loggerRuntime) coreEffect

-- | Runs node language.
runNodeL :: NodeRuntime -> L.NodeL TestWorld a -> IO a
runNodeL nodeRt = foldFree (interpretNodeL nodeRt)
