module Enecuum.Framework.Runtime where

import           Enecuum.Prelude

import           Control.Concurrent.STM.TChan
import qualified Data.Map            as Map

import           Enecuum.Core.Runtime (CoreRuntime)
import           Enecuum.Core.HGraph.Internal.Impl (initHGraph)
import qualified Enecuum.Domain as D
import qualified Network.Socket as S

-- TODO: the same types as in test runtime. Unify it.
data VarHandle = VarHandle D.VarId (TVar Any)
type NodeState = TMVar (Map.Map D.VarId VarHandle)

data NodeRuntime = NodeRuntime
    { _coreRuntime  :: CoreRuntime
    , _graph        :: D.TGraph D.NodeContent
    , _servers      :: TVar (Map D.PortNumber (TChan D.ServerComand))
    , _idCounter    :: TMVar Int              -- ^ ID counter. Used to generate VarIds, ProcessIds.
    , _state        :: NodeState              -- ^ State of node.
    , _nodeTag      :: TVar Text
    , _processes    :: TVar (Map D.ProcessId ThreadId)
    , _tcpConnects  :: TVar (Map D.Address (D.ConnectionVar D.Tcp))
    , _udpConnects  :: TVar (Map D.Address (D.ConnectionVar D.Udp))
    , _storyPaths   :: Map Text String
    }

createNodeRuntime :: CoreRuntime -> Map Text String -> IO NodeRuntime
createNodeRuntime coreRt paths =
    NodeRuntime
        <$> pure coreRt
        <*> initHGraph
        <*> newTVarIO mempty
        <*> newTMVarIO 0
        <*> newTMVarIO mempty
        <*> newTVarIO ""
        <*> newTVarIO mempty
        <*> newTVarIO mempty
        <*> newTVarIO mempty
        <*> pure paths

getNextId :: NodeRuntime -> STM Int
getNextId nodeRt = do
    number <- takeTMVar $ _idCounter nodeRt
    putTMVar (_idCounter nodeRt) $ number + 1
    pure number
