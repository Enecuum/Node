{-# LANGUAGE PackageImports #-}

module Enecuum.Framework.Runtime where

import qualified Data.Map                           as Map
import           Data.Aeson                         (Value)
import qualified "rocksdb-haskell" Database.RocksDB as Rocks
import           Enecuum.Core.HGraph.Internal.Impl  (initHGraph)
import           Enecuum.Core.Runtime               (CoreRuntime, VarHandle)
import qualified Network.Socket                     as S
import qualified Enecuum.Domain                     as D
import           Enecuum.Prelude
import qualified Enecuum.Core.Runtime               as R


class AsNativeConnection a where
    data family NativeConnection a
    getConnection :: NativeConnection a -> D.Connection a
    getSocketVar  :: NativeConnection a -> TMVar S.Socket
    getReaderId   :: NativeConnection a -> ThreadId

type Handler protocol  = Value -> D.Connection protocol -> IO ()
type Handlers protocol = Map Text (Handler protocol)
data ServerHandle = ServerHandle (TMVar S.Socket) ThreadId

type ConnectCounter = IORef D.ConnectId

type NodeState = TMVar (Map.Map D.VarId VarHandle)

data DBHandle  = DBHandle
    { _db    :: Rocks.DB
    , _mutex :: MVar ()
    }

data WorkerFlow
    = WContinue
    | WFinish 

data WorkerState
    = WOk
    | WWarning Text
    | WError Text

type WorkerAction = (WorkerFlow, WorkerState)

type Connections protocol = Map (D.Connection protocol) (NativeConnection protocol)
type ConnectionsVar protocol = TMVar (Connections protocol)
type TcpConnections = Connections D.Tcp
type UdpConnections = Connections D.Udp

data NodeRuntime = NodeRuntime
    { _coreRuntime      :: CoreRuntime
    , _graph            :: D.TGraph D.NodeContent
    , _servers          :: TMVar (Map D.PortNumber ServerHandle)
    , _connectCounter   :: ConnectCounter
    , _nodeTag          :: TVar Text
    , _processes        :: TVar (Map D.ProcessId ThreadId)
    , _tcpConnects      :: ConnectionsVar D.Tcp
    , _udpConnects      :: ConnectionsVar D.Udp
    , _storyPaths       :: Map Text String
    , _databases        :: TVar (Map FilePath DBHandle)
    }

createNodeRuntime :: CoreRuntime -> Map Text String -> IO NodeRuntime
createNodeRuntime coreRt paths =
    NodeRuntime
        <$> pure coreRt
        <*> initHGraph
        <*> newTMVarIO mempty
        <*> newIORef   0
        <*> newTVarIO ""
        <*> newTVarIO mempty
        <*> newTMVarIO mempty
        <*> newTMVarIO mempty
        <*> pure paths
        <*> newTVarIO mempty

showWorkerState :: R.RuntimeLogger -> WorkerState -> IO ()
showWorkerState _       WOk           = pure ()
showWorkerState logger (WWarning msg) = R.logWarning' logger msg
showWorkerState logger (WError   msg) = R.logError'   logger msg

withWorkerAction :: R.RuntimeLogger -> WorkerAction -> IO () -> IO ()
withWorkerAction logger (WContinue, st) cont = showWorkerState logger st >> cont
withWorkerAction logger (WFinish,   st) _    = showWorkerState logger st

