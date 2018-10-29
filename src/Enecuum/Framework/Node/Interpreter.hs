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

interpretNodeL nodeRt (L.OpenTcpConnection addr initScript next) =
    next <$> openConnection nodeRt addr initScript

interpretNodeL nodeRt (L.OpenUdpConnection addr initScript next) =
    next <$> openConnection nodeRt addr initScript

interpretNodeL nodeRt (L.CloseTcpConnection (D.Connection addr) next) =
    next <$> closeConnection (nodeRt ^. RLens.tcpConnects) addr

interpretNodeL nodeRt (L.CloseUdpConnection (D.Connection addr) next) =
    next <$> closeConnection (nodeRt ^. RLens.udpConnects) addr

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
        Left (err :: SomeException) -> pure $ next $ Left $ D.DBError D.DBSystemError (show err)
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

type F f a = a -> f a

class ConnectsLens a where
    connectsLens
        :: Functor f
        => F f (TVar (Map D.Address (D.ConnectionVar a)))
        -> NodeRuntime
        -> f NodeRuntime

instance ConnectsLens D.Udp where
    connectsLens = RLens.udpConnects

instance ConnectsLens D.Tcp where
    connectsLens = RLens.tcpConnects


closeConnection
    :: (Con.NetworkConnection protocol, Ord k)
    => TVar (Map k (D.ConnectionVar protocol)) -> k -> IO ()
closeConnection var addr = atomically $ do
    m <- readTVar var
    whenJust (m ^. at addr) $ \con -> do
        Con.close con
        modifyTVar var $ M.delete addr

openConnection
    :: forall k a1 a2 (a3 :: k).(ConnectsLens a1, Con.NetworkConnection a1)
    => NodeRuntime -> D.Address -> L.NetworkHandlerL a1 L.NodeL a2 -> IO (D.Connection a3)
openConnection nodeRt addr initScript = do
    m <- atomically $ newTVar mempty
    _ <- Net.runNetworkHandlerL m initScript
    handlers <- readTVarIO m
    newCon   <- Con.openConnect
        addr
        ((\f a b -> runNodeL nodeRt $ f a b) <$> handlers)
        (logError' nodeRt)
    insertConnect (nodeRt ^. connectsLens) addr newCon
    pure $ D.Connection addr

insertConnect :: Con.NetworkConnection a => TVar (Map D.Address (D.ConnectionVar a)) -> D.Address -> D.ConnectionVar a -> IO ()
insertConnect m addr newCon = atomically $ do
    conns <- readTVar m
    whenJust (conns ^. at addr) Con.close
    modifyTVar m $ M.insert addr newCon

setServerChan :: TVar (Map S.PortNumber (TChan D.ServerComand)) -> S.PortNumber -> TChan D.ServerComand -> STM ()
setServerChan servs port chan = do
    serversMap <- readTVar servs
    whenJust (serversMap ^. at port) Con.stopServer
    modifyTVar servs (M.insert port chan)

-- | Runs node language. Runs interpreters for the underlying languages.
runNodeL :: NodeRuntime -> L.NodeL a -> IO a
runNodeL nodeRt = foldFree (interpretNodeL nodeRt)

logError' :: NodeRuntime -> Log.Message -> IO ()
logError' nodeRt = runNodeL nodeRt . L.logError
