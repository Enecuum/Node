{-# LANGUAGE PackageImports #-}

module Enecuum.Framework.Runtime where

import           Control.Concurrent.STM.TChan
import qualified Data.Map                           as Map
import qualified "rocksdb-haskell" Database.RocksDB as Rocks
import           Enecuum.Core.HGraph.Internal.Impl (initHGraph)
import           Enecuum.Core.Runtime              (CoreRuntime)
import qualified Enecuum.Domain                    as D
import           Enecuum.Prelude

data DBHandle  = DBHandle
    { _db    :: Rocks.DB
    , _mutex :: MVar ()
    }

data NodeRuntime = NodeRuntime
    { _coreRuntime :: CoreRuntime
    , _graph       :: D.TGraph D.NodeContent
    , _servers     :: TVar (Map D.PortNumber (TChan D.ServerComand))
    , _nodeTag     :: TVar Text
    , _processes   :: TVar (Map D.ProcessId ThreadId)
    , _tcpConnects :: TVar (Map D.Address (D.ConnectionVar D.Tcp))
    , _udpConnects :: TVar (Map D.Address (D.ConnectionVar D.Udp))
    , _storyPaths  :: Map Text String
    , _databases   :: TVar (Map FilePath DBHandle)
    }

createNodeRuntime :: CoreRuntime -> Map Text String -> IO NodeRuntime
createNodeRuntime coreRt paths =
    NodeRuntime
        <$> pure coreRt
        <*> initHGraph
        <*> newTVarIO mempty
        <*> newTVarIO ""
        <*> newTVarIO mempty
        <*> newTVarIO mempty
        <*> newTVarIO mempty
        <*> pure paths
        <*> newTVarIO mempty
