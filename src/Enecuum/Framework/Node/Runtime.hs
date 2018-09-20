
module Enecuum.Framework.Node.Runtime where

import    Enecuum.Prelude
import    Enecuum.Core.Runtime
import    Enecuum.Domain
import    Data.HGraph.THGraph
import    Enecuum.Core.HGraph.Interpreter

data NodeRuntime = NodeRuntime
    { _coreRuntime  :: CoreRuntime
    , _graphRuntime :: TVar (THGraph Transaction)
    }

makeNodeRuntime :: IO NodeRuntime
makeNodeRuntime = NodeRuntime makeCoreRuntime <$> initHGraph