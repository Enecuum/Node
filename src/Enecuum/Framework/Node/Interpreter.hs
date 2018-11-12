{-# LANGUAGE TypeInType #-}
{-# LANGUAGE PackageImports #-}

module Enecuum.Framework.Node.Interpreter where

import           Enecuum.Prelude
import qualified "rocksdb-haskell" Database.RocksDB       as Rocks

import           Control.Concurrent.STM.TChan
import qualified Data.Map                                         as M
import           Enecuum.Core.HGraph.Internal.Impl
import qualified Enecuum.Core.Types                               as D
import qualified Enecuum.Core.Lens                                as Lens
import           Enecuum.Core.HGraph.Interpreters.IO
import qualified Enecuum.Core.Interpreters                        as Impl
import qualified Enecuum.Core.Language                            as L
import qualified Enecuum.Framework.Domain.Networking              as D
import qualified Enecuum.Framework.Handler.Network.Interpreter    as Net
import qualified Enecuum.Framework.Networking.Internal.Connection as Con
import qualified Enecuum.Framework.Networking.Interpreter         as Impl
import qualified Enecuum.Framework.Language                       as L
import qualified Enecuum.Framework.RLens                          as RLens
import qualified Enecuum.Framework.Runtime                        as R
import           Enecuum.Framework.Runtime                        (NodeRuntime)
import qualified Enecuum.Framework.State.Interpreter              as Impl
import qualified Enecuum.Core.Types.Logger as Log
import qualified Network.Socket                                   as S
import           Enecuum.Framework.Networking.Internal.Connection (ServerHandle (..))

runDatabase :: R.DBHandle -> L.DatabaseL db a -> IO a
runDatabase dbHandle action = do
    void $ takeMVar $ dbHandle ^. RLens.mutex
    res <- Impl.runDatabaseL (dbHandle ^. RLens.db) action
    putMVar (dbHandle ^. RLens.mutex) ()
    pure res

-- | Interpret NodeL.
interpretNodeL :: NodeRuntime -> L.NodeF a -> IO a
interpretNodeL nodeRt (L.EvalStateAtomically statefulAction next) =
    next <$> atomically (Impl.runStateL nodeRt statefulAction)

interpretNodeL _      (L.EvalGraphIO gr act next       ) = next <$>  (runHGraphIO gr act)

interpretNodeL nodeRt (L.EvalNetworking networking next) = next <$>  (Impl.runNetworkingL nodeRt networking)

interpretNodeL nodeRt (L.EvalCoreEffectNodeF coreEffects next) =
    next <$>  (Impl.runCoreEffect (nodeRt ^. RLens.coreRuntime) coreEffects)

interpretNodeL nodeRt (L.OpenTcpConnection serverAddr handlersScript next) =
    next <$> openConnection nodeRt (nodeRt ^. RLens.tcpConnects) serverAddr handlersScript

interpretNodeL nodeRt (L.OpenUdpConnection serverAddr handlersScript next) =
    next <$> openConnection nodeRt (nodeRt ^. RLens.udpConnects) serverAddr handlersScript

interpretNodeL nodeRt (L.CloseTcpConnection conn next) =
    next <$> closeConnection (nodeRt ^. RLens.tcpConnects) conn

interpretNodeL nodeRt (L.CloseUdpConnection conn next) =
    next <$> closeConnection (nodeRt ^. RLens.udpConnects) conn

interpretNodeL nodeRt (L.InitDatabase cfg next) = do
    let path = cfg ^. Lens.path
    let opts = Rocks.defaultOptions
            { Rocks.createIfMissing = cfg ^. Lens.options . Lens.createIfMissing
            , Rocks.errorIfExists   = cfg ^. Lens.options . Lens.errorIfExists
            }
    -- TODO: FIXME: check what exceptions may be thrown here and handle it correctly.
    -- TODO: ResourceT usage.
    eDb <- try $ Rocks.open path opts
    case eDb of
        Left (err :: SomeException) -> pure $ next $ Left $ D.DBError D.SystemError (show err)
        Right db -> do
            -- DB single entry point: worker.
            mutex <- newMVar ()
            let dbHandle = R.DBHandle db mutex
            -- Registering DB
            atomically $ modifyTVar (nodeRt ^. RLens.databases) (M.insert path dbHandle)
            pure $ next $ Right $ D.Storage path

interpretNodeL nodeRt (L.EvalDatabase storage action next) = do
    dbs <- readTVarIO $ nodeRt ^. RLens.databases
    case M.lookup (storage ^. Lens.path) dbs of
        Nothing       -> error $ "Impossible: DB is not registered: " +|| storage ^. Lens.path ||+ "."
        Just dbHandle -> do
            r <- runDatabase dbHandle action
            pure $ next r

interpretNodeL _ (L.NewGraph next) = next <$> initHGraph

closeConnection
    :: Con.NetworkConnection protocol
    => R.ConnectionsVar protocol
    -> D.Connection protocol
    -> IO ()
closeConnection connectionsVar conn = do
    trace_ "[closeConnection-high-level] closing high-level conn. Taking connections"
    connections <- atomically $ takeTMVar connectionsVar
    case M.lookup conn connections of
        Nothing -> do
            trace_ "[closeConnection-high-level] high-level conn not registered. Releasing connections"
            atomically $ putTMVar connectionsVar connections
        Just nativeConn -> do
            trace_ "[closeConnection-high-level] closing low-level conn"
            Con.close nativeConn
            trace_ "[closeConnection-high-level] deleting conn, releasing connections"
            let newConnections = M.delete conn connections
            atomically $ putTMVar connectionsVar newConnections

-- TODO: need to delete old connection if it's dead?
openConnection
    :: Con.NetworkConnection protocol
    => R.NodeRuntime
    -> R.ConnectionsVar protocol
    -> D.Address
    -> L.NetworkHandlerL protocol L.NodeL ()
    -> IO (Maybe (D.Connection protocol))
openConnection nodeRt connectionsVar serverAddr handlersScript = do
    trace_ "[openConnection-high-level] opening high-level conn. Taking connections"
    connections <- atomically $ takeTMVar connectionsVar

    m <- newTVarIO mempty
    _ <- Net.runNetworkHandlerL m handlersScript
    handlers <- readTVarIO m

    trace_ "[openConnection-high-level] opening low-level conn"
    mbConnections <- Con.open serverAddr ((\f a b -> runNodeL nodeRt $ f a b) <$> handlers)

    case mbConnections of
        Nothing -> do
            trace_ "[openConnection-high-level] failed opening low-level conn. Releasing connections"
            atomically $ putTMVar connectionsVar connections
            pure Nothing
        Just (conn, nativeConn) -> do
            -- This probably should not happen because bound address won't be repeating.
            -- But when it is, it means we haven't closed prev connection and haven't unregistered it.
            -- TODO: check isClosed in the appropriate places.
            when (conn `M.member` connections) $ trace_ $ "[openConnection-high-level] ERROR: connection exists: " <> show conn

            trace_ $ "[openConnection-high-level] low-level conn opened. Bound addr: " <> show conn
            trace_ $ "Registering. Releasing connections"
            let newConns = M.insert conn nativeConn connections
            atomically $ putTMVar connectionsVar newConns
            pure $ Just conn


-- This is all wrong, including invalid usage of TChan and other issues.
-- making it IO temp (which is even more wrong), but it needs to be deleted.
setServerChan :: TVar (Map S.PortNumber ServerHandle) -> S.PortNumber -> TChan D.ServerComand -> IO ()
setServerChan servs port chan = do
    serversMap <- readTVarIO servs
    whenJust (serversMap ^. at port) Con.stopServer
    atomically $ modifyTVar servs (M.insert port (OldServerHandle chan))

-- | Runs node language. Runs interpreters for the underlying languages.
runNodeL :: NodeRuntime -> L.NodeL a -> IO a
runNodeL nodeRt = foldFree (interpretNodeL nodeRt)

logError' :: NodeRuntime -> Log.Message -> IO ()
logError' nodeRt = runNodeL nodeRt . L.logError
