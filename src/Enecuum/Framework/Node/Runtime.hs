
module Enecuum.Framework.Node.Runtime where

import           Enecuum.Prelude

import           Data.HGraph.THGraph (THGraph)

import           Enecuum.Core.Runtime (CoreRuntime, createCoreRuntime)
import           Enecuum.Core.HGraph.Internal.Impl (initHGraph)
import qualified Enecuum.Domain as D

data NodeRuntime = NodeRuntime
    { _coreRuntime  :: CoreRuntime
    , _graph        :: TVar (THGraph D.Transaction)
    }

createNodeRuntime :: IO NodeRuntime
createNodeRuntime = NodeRuntime <$> createCoreRuntime <*> initHGraph
