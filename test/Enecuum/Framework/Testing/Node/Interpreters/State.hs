module Enecuum.Framework.Testing.Node.Interpreters.State where

import Enecuum.Prelude

import           Control.Monad.Free                                     (foldFree)

import qualified Enecuum.Language                                       as L
import           Enecuum.Core.HGraph.Interpreter                        (runHGraph)

import qualified Enecuum.Core.Testing.Runtime.Interpreters              as Impl
import           Enecuum.Framework.Testing.Types
import qualified Enecuum.Framework.Testing.Lens                         as RLens
import qualified Enecuum.Framework.Testing.Node.Interpreters.Networking as Impl

-- | Interpret StateL.
interpretStateL :: NodeRuntime -> L.StatF a -> IO a

interpretStateL nodeRt (L.NewVar val next) =
  next <$> Impl.runNetworkingL nodeRt networkingAction

  -- | Create variable.
  NewVar :: a -> (D.StateVar a -> next) -> StateF next
  -- | Read variable.
  ReadVar :: D.StateVar a -> (a -> next) -> StateF next
  -- | Write variable.
  WriteVar :: D.StateVar a -> a ->(() -> next) -> StateF next
  -- | Eval graph.
  EvalGraph :: LGraphModel a -> (a -> next) -> StateF next
  -- | Eval core effect.
  EvalCoreEffectStateF :: L.CoreEffectModel a -> (a -> next) -> StateF next



interpretStateL nodeRt (L.EvalGraph graphAction next) = do
  Impl.runLoggerL (nodeRt ^. RLens.loggerRuntime) $ L.logInfo "L.EvalGraph"
  next <$> runHGraph (nodeRt ^. RLens.graph) graphAction

interpretStateL nodeRt (L.EvalCoreEffectNodeF coreEffect next) =
  next <$> Impl.runCoreEffectModel (nodeRt ^. RLens.loggerRuntime) coreEffect

-- | Runs node model.
runNodeModel :: NodeRuntime -> L.NodeModel a -> IO a
runNodeModel nodeRt = foldFree (interpretStateL nodeRt)
