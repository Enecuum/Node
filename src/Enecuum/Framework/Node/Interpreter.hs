{-# LANGUAGE TypeInType #-}
{-# LANGUAGE PackageImports #-}

module Enecuum.Framework.Node.Interpreter where

import           Enecuum.Prelude
import qualified "rocksdb-haskell" Database.RocksDB       as Rocks

import qualified Data.Map                                         as M
import           Enecuum.Core.HGraph.Internal.Impl
import qualified Enecuum.Core.Types                               as D
import qualified Enecuum.Core.Lens                                as Lens
import           Enecuum.Core.HGraph.Interpreters.IO
import qualified Enecuum.Core.Interpreters                        as Impl
import qualified Enecuum.Core.Runtime                             as R
import qualified Enecuum.Core.Language                            as L
import qualified Enecuum.Framework.Domain.Networking              as D
import qualified Enecuum.Framework.Handler.Network.Interpreter    as Net
import qualified Enecuum.Framework.Networking.Internal.Connection as Conn
import qualified Enecuum.Framework.Networking.Interpreter         as Impl
import qualified Enecuum.Framework.Language                       as L
import qualified Enecuum.Framework.RLens                          as RLens
import qualified Enecuum.Core.RLens                               as RLens
import qualified Enecuum.Framework.Runtime                        as R
import           Enecuum.Framework.Runtime                        (NodeRuntime)
import qualified Enecuum.Framework.State.Interpreter              as Impl

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

interpretNodeL _      (L.EvalGraphIO gr act next       ) = next <$> runHGraphIO gr act

interpretNodeL nodeRt (L.EvalNetworking networking next) = next <$> Impl.runNetworkingL nodeRt networking

interpretNodeL nodeRt (L.EvalCoreEffectNodeF coreEffects next) =
    next <$> Impl.runCoreEffect (nodeRt ^. RLens.coreRuntime) coreEffects

interpretNodeL nodeRt (L.OpenTcpConnection serverAddr handlersScript next) =
    next <$> openConnection nodeRt (nodeRt ^. RLens.tcpConnects) serverAddr handlersScript

interpretNodeL nodeRt (L.OpenUdpConnection serverAddr handlersScript next) =
    next <$> openConnection nodeRt (nodeRt ^. RLens.udpConnects) serverAddr handlersScript

interpretNodeL nodeRt (L.CloseTcpConnection conn next) =
    next <$> closeConnection nodeRt (nodeRt ^. RLens.tcpConnects) conn

interpretNodeL nodeRt (L.CloseUdpConnection conn next) =
    next <$> closeConnection nodeRt (nodeRt ^. RLens.udpConnects) conn

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
    :: R.NodeRuntime
    -> Conn.NetworkConnection protocol
    => R.ConnectionsVar protocol
    -> D.Connection protocol
    -> IO ()
closeConnection nodeRt connectionsVar conn = do
    let logger = R.mkRuntimeLogger $ nodeRt ^. RLens.coreRuntime . RLens.loggerRuntime

    connections <- atomically $ takeTMVar connectionsVar
    case M.lookup conn connections of
        Nothing -> atomically $ putTMVar connectionsVar connections
        Just nativeConn -> do
            Conn.close logger nativeConn
            let newConnections = M.delete conn connections
            atomically $ putTMVar connectionsVar newConnections

openConnection
    :: Conn.NetworkConnection protocol
    => R.NodeRuntime
    -> R.ConnectionsVar protocol
    -> D.Address
    -> L.NetworkHandlerL protocol L.NodeL ()
    -> IO (Maybe (D.Connection protocol))
openConnection nodeRt connectionsVar serverAddr handlersScript = do
    let logger = R.mkRuntimeLogger $ nodeRt ^. RLens.coreRuntime . RLens.loggerRuntime

    connections <- atomically $ takeTMVar connectionsVar

    m <- newTVarIO mempty
    _ <- Net.runNetworkHandlerL m handlersScript
    handlers <- readTVarIO m

    mbNativeConn <- Conn.open
        logger
        (nodeRt ^. RLens.connectCounter)
        serverAddr
        ((\f a b -> runNodeL nodeRt $ f a b) <$> handlers)

    case mbNativeConn of
        Nothing -> do
            atomically $ putTMVar connectionsVar connections
            pure Nothing
        Just nativeConn -> do
            let conn = R.getConnection nativeConn

            -- This probably should not happen because bound address won't be repeating.
            -- But when it is, it means we haven't closed prev connection and haven't unregistered it (bug).
            when (conn `M.member` connections) $ error $ "Impossible: connection exists: " <> show conn

            let newConns = M.insert conn nativeConn connections
            atomically $ putTMVar connectionsVar newConns
            pure $ Just conn

-- | Runs node language. Runs interpreters for the underlying languages.
runNodeL :: NodeRuntime -> L.NodeL a -> IO a
runNodeL nodeRt = foldFree (interpretNodeL nodeRt)
