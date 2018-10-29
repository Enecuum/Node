{-# LANGUAGE TypeInType #-}
module Enecuum.Framework.Node.Interpreter where

import           Enecuum.Prelude

import           Control.Concurrent.STM.TChan
import qualified Data.Map                                         as M
import           Enecuum.Core.HGraph.Internal.Impl
import           Enecuum.Core.HGraph.Interpreters.IO
import qualified Enecuum.Core.Interpreters                        as Impl
import qualified Enecuum.Core.Logger.Language                     as L
import qualified Enecuum.Framework.Domain.Networking              as D
import qualified Enecuum.Framework.Handler.Network.Interpreter    as Net
import qualified Enecuum.Framework.Networking.Internal.Connection as Con
import qualified Enecuum.Framework.Networking.Interpreter         as Impl
import qualified Enecuum.Framework.Language                       as L
import qualified Enecuum.Framework.RLens                          as RLens
import           Enecuum.Framework.Runtime                        (NodeRuntime)
import qualified Enecuum.Framework.State.Interpreter              as Impl
import qualified Enecuum.Core.Types.Logger as Log
import qualified Network.Socket                                   as S

-- | Interpret NodeL.
interpretNodeL :: NodeRuntime -> L.NodeF a -> IO a
interpretNodeL nodeRt (L.EvalStateAtomically statefulAction next) =
    next <$> atomically (Impl.runStateL nodeRt statefulAction)

interpretNodeL _      (L.EvalGraphIO gr act next       ) = next <$> runHGraphIO gr act

interpretNodeL nodeRt (L.EvalNetworking networking next) = next <$> Impl.runNetworkingL nodeRt networking

interpretNodeL nodeRt (L.EvalCoreEffectNodeF coreEffects next) =
    next <$> Impl.runCoreEffect (nodeRt ^. RLens.coreRuntime) coreEffects

interpretNodeL nodeRt (L.OpenTcpConnection addr initScript next) =
    next <$> openConnection nodeRt addr initScript

interpretNodeL nodeRt (L.OpenUdpConnection addr initScript next) =
    next <$> openConnection nodeRt addr initScript

interpretNodeL nodeRt (L.CloseTcpConnection (D.Connection addr) next) =
    next <$> closeConnection (nodeRt ^. RLens.tcpConnects) addr 

interpretNodeL nodeRt (L.CloseUdpConnection (D.Connection addr) next) =
    next <$> closeConnection (nodeRt ^. RLens.udpConnects) addr

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
