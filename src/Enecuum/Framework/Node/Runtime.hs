
module Enecuum.Framework.Node.Runtime where

import           Enecuum.Prelude

import           Data.HGraph.THGraph (THGraph)

import           Enecuum.Core.Runtime (CoreRuntime, createCoreRuntime)
import           Enecuum.Core.HGraph.Internal.Impl (initHGraph)
import qualified Enecuum.Domain as D
import    Enecuum.Legacy.Service.Network.Base
import    Control.Concurrent.STM.TChan
import    Enecuum.Legacy.Refact.Network.Server
import    Control.Monad.STM

data NodeRuntime = NodeRuntime
    { _coreRuntime  :: CoreRuntime
    , _graph        :: TVar (THGraph D.Transaction)
    , _servers      :: TVar (Map PortNumber (TChan ServerComand))
    }

createNodeRuntime :: IO NodeRuntime
createNodeRuntime = NodeRuntime
    <$> createCoreRuntime
    <*> initHGraph
    <*> newTVarIO mempty
