{-# LANGUAGE PackageImports #-}

module Enecuum.Framework.Node.Interpreter where

import           Enecuum.Prelude
import           Control.Concurrent.STM.TChan
import qualified Control.Monad.Trans.Resource             as Res
import qualified "rocksdb-haskell" Database.RocksDB       as Rocks
import qualified Data.Map                                 as M
import qualified Network.Socket  as S

import qualified Enecuum.Framework.State.Language         as L
import qualified Enecuum.Framework.Node.Language          as L
import qualified Enecuum.Core.Logger.Language             as L
import qualified Enecuum.Framework.Networking.Interpreter as Impl
import           Enecuum.Framework.Runtime                (NodeRuntime)
import qualified Enecuum.Core.Interpreters                as Impl
import qualified Enecuum.Framework.State.Interpreter      as Impl
import qualified Enecuum.Framework.RLens                  as RLens
import qualified Enecuum.Core.Lens                        as Lens

import qualified Enecuum.Framework.Networking.Internal.Tcp.Server      as Tcp
import qualified Enecuum.Framework.Networking.Internal.Tcp.Connection  as Tcp

import qualified Enecuum.Framework.Networking.Internal.Udp.Server      as Udp
import qualified Enecuum.Framework.Networking.Internal.Udp.Connection  as Udp
import qualified Enecuum.Framework.Handler.Network.Interpreter         as Net
import qualified Enecuum.Framework.Networking.Internal.Connection      as Con

import qualified Enecuum.Core.Types                  as D
import qualified Enecuum.Framework.Domain.Networking as D
import           Enecuum.Core.HGraph.Interpreters.IO
import           Enecuum.Core.HGraph.Internal.Impl


-- | Interpret NodeL.
interpretNodeL :: NodeRuntime -> L.NodeF a -> Res.ResIO a
interpretNodeL nodeRt (L.EvalStateAtomically statefulAction next) =
    next <$> atomically (Impl.runStateL nodeRt statefulAction)

interpretNodeL _      (L.EvalGraphIO gr act next       ) = next <$> liftIO (runHGraphIO gr act)

interpretNodeL nodeRt (L.EvalNetworking networking next) = next <$> liftIO (Impl.runNetworkingL nodeRt networking)

interpretNodeL nodeRt (L.EvalCoreEffectNodeF coreEffects next) =
    next <$> liftIO (Impl.runCoreEffect (nodeRt ^. RLens.coreRuntime) coreEffects)

interpretNodeL nodeRt (L.OpenTcpConnection addr initScript next) = 
    next <$> openConnection nodeRt addr initScript

interpretNodeL nodeRt (L.OpenUdpConnection addr initScript next) =
    next <$> openConnection nodeRt addr initScript

interpretNodeL nodeRt (L.CloseTcpConnection (D.Connection addr) next) =
    next <$> liftIO (closeConnection nodeRt addr RLens.tcpConnects)

interpretNodeL nodeRt (L.CloseUdpConnection (D.Connection addr) next) =
    next <$> liftIO (closeConnection nodeRt addr RLens.udpConnects)

interpretNodeL nodeRt (L.InitDatabase cfg next) = do
    let path = cfg ^. Lens.path
    let opts = Rocks.defaultOptions
            { Rocks.createIfMissing = cfg ^. Lens.options . Lens.createIfMissing
            , Rocks.errorIfExists   = cfg ^. Lens.options . Lens.errorIfExists
            }
    -- TODO: FIXME: check what exceptions may be thrown here and handle it correctly.
    dbHandle <- Rocks.openBracket path opts
    liftIO $ atomically $ modifyTVar (nodeRt ^. RLens.databases) (M.insert path dbHandle)
    pure $ next $ Right $ D.Storage path

interpretNodeL _ (L.NewGraph next) = next <$> liftIO initHGraph

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

closeConnection nodeRt addr lens = atomically $ do
    m <- readTVar (nodeRt ^. lens)
    whenJust (m ^. at addr) $ \con -> do
        Con.close con
        modifyTVar (nodeRt ^. lens) $ M.delete addr
    
openConnection nodeRt addr initScript = do
    m <- liftIO $ atomically $ newTVar mempty
    liftIO $ Net.runNetworkHandlerL m initScript
    handlers <- liftIO $ readTVarIO m
    newCon   <- Con.openConnect
        addr
        ((\f a b -> runNodeL nodeRt $ f a b) <$> handlers)
        (logError' nodeRt)
    liftIO $ insertConnect (nodeRt ^. connectsLens) addr newCon
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
runNodeL :: NodeRuntime -> L.NodeL a -> Res.ResIO a
runNodeL nodeRt = foldFree (interpretNodeL nodeRt)

logError' nodeRt = runNodeL nodeRt . L.logError