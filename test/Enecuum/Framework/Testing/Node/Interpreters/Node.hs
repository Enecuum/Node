module Enecuum.Framework.Testing.Node.Interpreters.Node where

import Enecuum.Prelude

import           Control.Monad.Free                                     (foldFree)

import qualified Enecuum.Language                                       as L
import           Enecuum.Core.HGraph.Interpreter                        (runHGraph)

import qualified Enecuum.Core.Testing.Runtime.Interpreters              as Impl
import           Enecuum.Framework.Testing.Types
import qualified Enecuum.Framework.Testing.Lens                         as RLens
import qualified Enecuum.Framework.Testing.Node.Interpreters.Networking as Impl

-- | Interpret NodeL. Does nothing ATM.
interpretNodeL :: NodeRuntime -> L.NodeF a -> IO a

interpretNodeL nodeRt (L.EvalGraph graphAction next) = do
  Impl.runLoggerL (nodeRt ^. RLens.loggerRuntime) $ L.logInfo "L.EvalGraph"
  next <$> runHGraph (nodeRt ^. RLens.graph) graphAction

interpretNodeL nodeRt (L.EvalNetworking networkingAction next) =
  next <$> Impl.runNetworkingL nodeRt networkingAction

interpretNodeL nodeRt (L.EvalCoreEffectNodeF coreEffect next) =
  next <$> Impl.runCoreEffect (nodeRt ^. RLens.loggerRuntime) coreEffect

-- | Runs node language.
runNodeL :: NodeRuntime -> L.NodeL a -> IO a
runNodeL nodeRt = foldFree (interpretNodeL nodeRt)
