module Enecuum.Framework.Node.Interpreter where

import Enecuum.Prelude

import qualified Enecuum.Framework.State.Language         as L
import qualified Enecuum.Framework.Node.Language          as L
import qualified Enecuum.Framework.Networking.Interpreter as Impl
import           Enecuum.Framework.Runtime                (NodeRuntime)
import qualified Enecuum.Core.Interpreters                as Impl
import qualified Enecuum.Framework.State.Interpreter      as Impl
import qualified Enecuum.Framework.RLens                  as RLens


-- | Interpret NodeL.
interpretNodeL :: NodeRuntime -> L.NodeF a -> IO a
interpretNodeL nodeRt (L.EvalStateAtomically statefulAction next) =
    next <$> (atomically $ Impl.runStateL nodeRt statefulAction)

interpretNodeL _ (L.EvalGraphIO (L.GraphAction _ ioRunner act) next) =
  next <$> ioRunner act

interpretNodeL _ (L.EvalNetworking networking next) =
    next <$> Impl.runNetworkingL networking

interpretNodeL nodeRt (L.EvalCoreEffectNodeF coreEffects next) =
    next <$> Impl.runCoreEffect (nodeRt ^. RLens.coreRuntime) coreEffects

interpretNodeL nodeRt (L.StopNode next) = do
    atomically $ putTMVar (nodeRt ^. RLens.stopNode) True
    return $ next ()





{-
  fmap g (StopServing port next)                   = StopServing port                   (g . next)
  fmap g (OpenConnection a b next)                 = OpenConnection  a b                (g . next)
  fmap g (ServingMsg a b next)                     = ServingMsg a b                     (g . next)
  fmap g (CloseConnection a next)                  = CloseConnection a                  (g . next)
-}

--


-- | Runs node language. Runs interpreters for the underlying languages.
runNodeL :: NodeRuntime -> L.NodeL a -> IO a
runNodeL nodeRt = foldFree (interpretNodeL nodeRt)
