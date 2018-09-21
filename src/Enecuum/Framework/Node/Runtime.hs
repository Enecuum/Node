
module Enecuum.Framework.Node.Runtime where

import    Enecuum.Prelude
import    Enecuum.Core.Runtime
import    Enecuum.Domain
import    Data.HGraph.THGraph
import    Enecuum.Core.HGraph.Interpreter
import    Enecuum.Legacy.Service.Network.Base
import    Control.Concurrent.STM.TChan
import    Enecuum.Legacy.Refact.Network.Server
import    Control.Monad.STM

data NodeRuntime = NodeRuntime
    { _coreRuntime  :: CoreRuntime
    , _graphRuntime :: TVar (THGraph Transaction)
    , _servers      :: TVar (Map PortNumber (TChan ServerComand))
    }

makeNodeRuntime :: IO NodeRuntime
makeNodeRuntime = NodeRuntime makeCoreRuntime <$> initHGraph <*> newTVarIO mempty