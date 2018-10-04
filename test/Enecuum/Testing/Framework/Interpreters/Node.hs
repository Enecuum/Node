module Enecuum.Testing.Framework.Interpreters.Node where

import Enecuum.Prelude

import qualified Enecuum.Language                                  as L

import qualified Enecuum.Testing.RLens                             as RLens
import qualified Enecuum.Testing.Types                             as T
import qualified Enecuum.Testing.Core.Interpreters                 as Impl
import qualified Enecuum.Testing.Framework.Interpreters.Networking as Impl
import qualified Enecuum.Testing.Framework.Interpreters.State      as Impl

import           Enecuum.Core.HGraph.Internal.Impl
import           Enecuum.Core.HGraph.Interpreters.IO 
-- | Interpret NodeL.
interpretNodeL :: T.NodeRuntime -> L.NodeF a -> IO a

interpretNodeL nodeRt (L.EvalStateAtomically statefulAction next) = do
  next <$> (atomically $ Impl.runStateL nodeRt statefulAction)

interpretNodeL nodeRt (L.EvalGraphIO gr act next) = do
  next <$> runHGraphIO gr act

interpretNodeL nodeRt (L.EvalNetworking networkingAction next) =
  next <$> Impl.runNetworkingL nodeRt networkingAction

interpretNodeL nodeRt (L.EvalCoreEffectNodeF coreEffect next) =
  next <$> Impl.runCoreEffect (nodeRt ^. RLens.loggerRuntime) coreEffect

interpretNodeL nodeRt (L.OpenConnection addr handlers next) = do
  -- rt <- newMVar nodeRt
  -- pure $ next $ Just $ D.NetworkConnection $ TestConnection
    -- rt $ Connection (nodeRt ^. RLens.address) (cfg ^. Lens.address)
  error "OpenConnection not implemented."
interpretNodeL _ (L.NewGraph next) = next <$> initHGraph 

interpretNodeL nodeRt (L.CloseConnection _ next) =
  -- pure $ next ()
  error "CloseConnection not implemented."

-- interpretNetworkingL nodeRt (L.Send (D.NetworkConnection conn) req next) = do
--   -- next <$> D.sendRequest conn req
--   error "Send not implemented."

-- | Runs node language.
runNodeL :: T.NodeRuntime -> L.NodeL a -> IO a
runNodeL nodeRt = foldFree (interpretNodeL nodeRt)
