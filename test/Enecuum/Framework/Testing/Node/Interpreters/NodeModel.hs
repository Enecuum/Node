module Enecuum.Framework.Testing.Node.Interpreters.NodeModel where

import Enecuum.Prelude

import           Eff                                ( handleRelay )

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L
import qualified Enecuum.Framework.Lens             as Lens
import           Enecuum.Core.HGraph.Interpreter    (runHGraph)
import           Enecuum.Core.HGraph.Language       ( W (..) )

import           Enecuum.Framework.Testing.Types
import qualified Enecuum.Framework.Testing.Lens              as RLens

import           Enecuum.Framework.Testing.Node.Interpreters.NetworkModel
import           Enecuum.Framework.Testing.Node.Interpreters.Networking

-- | Interpret NodeL. Does nothing ATM.
interpretNodeL
  :: NodeRuntime
  -> L.NodeL a
  -> Eff '[L.NetworkingL, L.NetworkSyncL, L.NetworkListeningL, L.NetworkSendingL, L.LoggerL, SIO, Exc SomeException] a
interpretNodeL nodeRt (L.EvalGraphAction graphAction) = do
  L.logInfo "L.EvalGraph"
  safeIO $ runHGraph (nodeRt ^. RLens.graph) graphAction

-- | Runs node model. Runs interpreters for the underlying languages.
runNodeModel
  :: NodeRuntime
  -> Eff L.NodeModel a
  -> Eff '[L.LoggerL, SIO, Exc SomeException] a
runNodeModel nodeRt
  = handleRelay pure ( (>>=) . interpretNetworkSendingL nodeRt )
  . handleRelay pure ( (>>=) . interpretNetworkListeningL nodeRt )
  . handleRelay pure ( (>>=) . interpretNetworkSyncL nodeRt )
  . handleRelay pure ( (>>=) . interpretNetworkingL nodeRt )
  . handleRelay pure ( (>>=) . interpretNodeL nodeRt )
