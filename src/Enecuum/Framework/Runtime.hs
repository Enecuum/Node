{-# LANGUAGE PackageImports #-}

module Enecuum.Framework.Runtime where

import qualified Data.Map                           as Map
import qualified "rocksdb-haskell" Database.RocksDB as Rocks
import           Enecuum.Core.HGraph.Internal.Impl (initHGraph)
import           Enecuum.Core.Runtime              (CoreRuntime)
import qualified Enecuum.Domain                    as D
import           Enecuum.Prelude
import           Enecuum.Framework.Networking.Internal.Connection (ServerHandle, NativeConnection)

data VarHandle = VarHandle D.VarId (TVar Any)
type NodeState = TMVar (Map.Map D.VarId VarHandle)

data DBHandle  = DBHandle
    { _db    :: Rocks.DB
    , _mutex :: MVar ()
    }

type Connections protocol = Map (D.Connection protocol) (NativeConnection protocol)
type ConnectionsVar protocol = TMVar (Connections protocol)
type TcpConnections = Connections D.Tcp
type UdpConnections = Connections D.Udp

data NodeRuntime = NodeRuntime
    { _coreRuntime :: CoreRuntime
    , _graph       :: D.TGraph D.NodeContent
    , _servers     :: TMVar (Map D.PortNumber ServerHandle)
    , _idCounter   :: TMVar Int              -- ^ ID counter. Used to generate VarIds, ProcessIds.
    , _state       :: NodeState              -- ^ State of node.
    , _nodeTag     :: TVar Text
    , _processes   :: TVar (Map D.ProcessId ThreadId)
    , _tcpConnects :: ConnectionsVar D.Tcp
    , _udpConnects :: ConnectionsVar D.Udp
    , _storyPaths  :: Map Text String
    , _databases   :: TVar (Map FilePath DBHandle)
    }

createNodeRuntime :: CoreRuntime -> Map Text String -> IO NodeRuntime
createNodeRuntime coreRt paths =
    NodeRuntime
        <$> pure coreRt
        <*> initHGraph
        <*> newTMVarIO mempty
        <*> newTMVarIO 0
        <*> newTMVarIO mempty
        <*> newTVarIO ""
        <*> newTVarIO mempty
        <*> newTMVarIO mempty
        <*> newTMVarIO mempty
        <*> pure paths
        <*> newTVarIO mempty

getNextId :: NodeRuntime -> STM Int
getNextId nodeRt = do
    number <- takeTMVar $ _idCounter nodeRt
    putTMVar (_idCounter nodeRt) $ number + 1
    pure number
