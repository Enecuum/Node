module Enecuum.Framework.Node.Interpreter where

import Enecuum.Prelude

import qualified Enecuum.Framework.State.Language         as L
import qualified Enecuum.Framework.Node.Language          as L
import qualified Enecuum.Framework.Networking.Interpreter as Impl
import           Enecuum.Framework.Runtime                (NodeRuntime)
import qualified Enecuum.Core.Interpreters                as Impl
import qualified Enecuum.Framework.State.Interpreter      as Impl
import qualified Enecuum.Framework.RLens                  as RLens
import           Control.Concurrent.STM.TChan
import           Enecuum.Legacy.Service.Network.Base
import qualified Enecuum.Framework.Networking.Internal.Tcp.Server      as Tcp
import qualified Enecuum.Framework.Networking.Internal.Tcp.Connection  as Tcp

import qualified Enecuum.Framework.Networking.Internal.Udp.Server      as Udp
import qualified Enecuum.Framework.Networking.Internal.Udp.Connection  as Udp
import qualified Enecuum.Framework.Handler.Network.Interpreter         as Net
import qualified Enecuum.Framework.Networking.Internal.Connection     as Con

import qualified Data.Map                                 as M
import qualified Enecuum.Framework.Domain.Networking as D
import           Enecuum.Core.HGraph.Interpreters.IO
import           Enecuum.Core.HGraph.Internal.Impl

-- | Interpret NodeL.
interpretNodeL :: NodeRuntime -> L.NodeF a -> IO a
interpretNodeL nodeRt (L.EvalStateAtomically statefulAction next) =
    next <$> (atomically $ Impl.runStateL nodeRt statefulAction)

interpretNodeL _      (L.EvalGraphIO gr act next       ) = next <$> runHGraphIO gr act

interpretNodeL nodeRt (L.EvalNetworking networking next) = next <$> Impl.runNetworkingL nodeRt networking

interpretNodeL nodeRt (L.EvalCoreEffectNodeF coreEffects next) =
    next <$> Impl.runCoreEffect (nodeRt ^. RLens.coreRuntime) coreEffects

interpretNodeL nodeRt (L.OpenTcpConnection addr initScript next) = do
    m <- atomically $ newTVar mempty
    Net.runNetworkHandlerL m initScript
    handlers <- readTVarIO m
    newCon   <- Con.openConnect addr ((\f a b -> runNodeL nodeRt $ f a b) <$> handlers)
    insertConnect (nodeRt ^. RLens.tcpConnects) addr newCon
    pure $ next (D.Connection addr)

interpretNodeL nodeRt (L.OpenUdpConnection addr initScript next) = do
    m <- atomically $ newTVar mempty
    Net.runNetworkHandlerL m initScript
    handlers <- readTVarIO m
    newCon   <- Udp.openConnect addr ((\f a b -> runNodeL nodeRt $ f a b) <$> handlers)
    insertUdpConnect (nodeRt ^. RLens.udpConnects) addr newCon
    pure $ next (D.Connection addr)

interpretNodeL nodeRt (L.CloseTcpConnection (D.Connection addr) next) = do
    atomically $ do
        m <- readTVar (nodeRt ^. RLens.tcpConnects)
        whenJust (m ^. at addr) $ \con -> do
            Con.close con
            modifyTVar (nodeRt ^. RLens.tcpConnects) $ M.delete addr
    pure $ next ()
--
interpretNodeL nodeRt (L.CloseUdpConnection (D.Connection addr) next) = do
    atomically $ do
        m <- readTVar (nodeRt ^. RLens.udpConnects)
        whenJust (m ^. at addr) $ \con -> do
            Udp.close con
            modifyTVar (nodeRt ^. RLens.udpConnects) $ M.delete addr
    pure $ next ()

interpretNodeL _ (L.NewGraph next) = next <$> initHGraph

insertConnect :: TVar (Map D.Address (D.ConnectionVar D.Tcp) ) -> D.Address -> D.ConnectionVar D.Tcp -> IO ()
insertConnect m addr newCon = atomically $ do
    conns <- readTVar $ m
    whenJust (conns ^. at addr) Con.close
    modifyTVar m $ M.insert addr newCon

insertUdpConnect :: TVar (Map D.Address (D.ConnectionVar D.Udp)) -> D.Address -> D.ConnectionVar D.Udp -> IO ()
insertUdpConnect m addr newCon = atomically $ do
    conns <- readTVar $ m
    whenJust (conns ^. at addr) Udp.close
    modifyTVar m $ M.insert addr newCon

setServerChan :: TVar (Map PortNumber (TChan D.ServerComand)) -> PortNumber -> TChan D.ServerComand -> STM ()
setServerChan servs port chan = do
    serversMap <- readTVar servs
    whenJust (serversMap ^. at port) Con.stopServer
    modifyTVar servs (M.insert port chan)

-- | Runs node language. Runs interpreters for the underlying languages.
runNodeL :: NodeRuntime -> L.NodeL a -> IO a
runNodeL nodeRt = foldFree (interpretNodeL nodeRt)
