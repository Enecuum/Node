module Enecuum.FunctionalTesting.Framework.Interpreters.Node where

import Enecuum.Prelude

import qualified Enecuum.Language                                  as L

import qualified Enecuum.FunctionalTesting.RLens                             as RLens
import qualified Enecuum.FunctionalTesting.Types                             as T
import qualified Enecuum.FunctionalTesting.Core.Interpreters                 as Impl
import qualified Enecuum.FunctionalTesting.Framework.Interpreters.Networking as Impl
import qualified Enecuum.FunctionalTesting.Framework.Interpreters.State      as Impl

-- | Interpret NodeL.
interpretNodeL :: T.NodeRuntime -> L.NodeF a -> IO a

interpretNodeL nodeRt (L.EvalStateAtomically statefulAction next) = do
  next <$> (atomically $ Impl.runStateL nodeRt statefulAction)

interpretNodeL nodeRt (L.EvalGraphIO (L.GraphAction _ ioRunner act) next) = do
  next <$> ioRunner act

interpretNodeL nodeRt (L.EvalNetworking networkingAction next) =
  next <$> Impl.runNetworkingL nodeRt networkingAction

interpretNodeL nodeRt (L.EvalCoreEffectNodeF coreEffect next) =
  next <$> Impl.runCoreEffect (nodeRt ^. RLens.loggerRuntime) coreEffect

-- | Runs node language.
runNodeL :: T.NodeRuntime -> L.NodeL a -> IO a
runNodeL nodeRt = foldFree (interpretNodeL nodeRt)
