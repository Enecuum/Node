module Enecuum.Framework.Runtime where

import           Enecuum.Prelude

import           Control.Concurrent.STM.TChan
import qualified Data.Map            as Map
import           Data.HGraph.THGraph (THGraph)

import           Enecuum.Core.Runtime (CoreRuntime)
import           Enecuum.Core.HGraph.Internal.Impl (initHGraph)
import qualified Enecuum.Domain as D
import           Enecuum.Legacy.Service.Network.Base
import           Enecuum.Framework.Networking.Internal.TCP.Server

-- TODO: the same types as in test runtime. Unify it.
data VarHandle = VarHandle D.VarId (TVar Any)
type NodeState = TMVar (Map.Map D.VarId VarHandle)

data NodeRuntime = NodeRuntime
    { _coreRuntime  :: CoreRuntime
    , _graph        :: TVar (THGraph D.Transaction)
    , _servers      :: TVar (Map PortNumber (TChan ServerComand))
    , _varCounter   :: TMVar Int              -- ^ Vars counter. Used to generate VarId.
    , _state        :: NodeState              -- ^ State of node.
    , _nodeTag      :: TVar Text
    , _stopNode     :: TMVar Bool
    , _connects     :: TVar (Map D.Address D.ConnectionImplementation)
    }

createNodeRuntime :: CoreRuntime -> IO NodeRuntime
createNodeRuntime coreRt = NodeRuntime
    <$> pure coreRt
    <*> initHGraph
    <*> newTVarIO mempty
    <*> newTMVarIO 0
    <*> newTMVarIO Map.empty
    <*> newTVarIO ""
    <*> (atomically newEmptyTMVar)
    <*> newTVarIO mempty

-- TODO: more wise clearing here.
clearNodeRuntime :: NodeRuntime -> IO ()
clearNodeRuntime _ = pure ()
