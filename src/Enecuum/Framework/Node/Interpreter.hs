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
import           Enecuum.Framework.Networking.Internal.TCP.Server
import           Enecuum.Framework.Networking.Internal.Internal  as I
import           Enecuum.Framework.MsgHandler.Interpreter
import qualified Data.Map                                 as M
import qualified Enecuum.Framework.Domain.Networking as D
-- | Interpret NodeL.
interpretNodeL :: NodeRuntime -> L.NodeF a -> IO a
interpretNodeL nodeRt (L.EvalStateAtomically statefulAction next) =
    next <$> (atomically $ Impl.runStateL nodeRt statefulAction)

interpretNodeL _ (L.EvalGraphIO (L.GraphAction _ ioRunner act) next) =
  next <$> ioRunner act

interpretNodeL nodeRt (L.EvalNetworking networking next) =
    next <$> Impl.runNetworkingL nodeRt networking

interpretNodeL nodeRt (L.EvalCoreEffectNodeF coreEffects next) =
    next <$> Impl.runCoreEffect (nodeRt ^. RLens.coreRuntime) coreEffects

interpretNodeL nodeRt (L.StopNode next) = do
    atomically $ putTMVar (nodeRt ^. RLens.stopNode) True
    return $ next ()

interpretNodeL nodeRt (L.OpenConnection addr initScript next) = do
    m <- atomically $ newTVar mempty
    runMsgHandlerL m initScript
    handlers <- readTVarIO m
    newCon <- I.openConnect addr ((\f a b -> runNodeL nodeRt $ f a b) <$> handlers)
    insertConnect (nodeRt ^. RLens.connects) addr newCon
    pure $ next (D.NetworkConnection addr)

interpretNodeL nodeRt (L.CloseConnection (D.NetworkConnection addr) next) = do
    atomically $ do
        m <- readTVar (nodeRt ^. RLens.connects)
        whenJust (m ^. at addr) $ \con -> do
            I.close con
            modifyTVar (nodeRt ^. RLens.connects) $ M.delete addr
    return $ next ()

insertConnect :: TVar (Map D.Address D.ConnectionImplementation) -> D.Address -> D.ConnectionImplementation -> IO ()
insertConnect m addr newCon = atomically $ do
    conns <- readTVar $ m
    whenJust (conns ^. at addr) I.close
    modifyTVar m $ M.insert addr newCon

setServerChan :: TVar (Map PortNumber (TChan ServerComand)) -> PortNumber -> TChan ServerComand -> STM ()
setServerChan servs port chan = do
    serversMap <- readTVar servs
    whenJust (serversMap ^. at port) stopServer
    modifyTVar servs (M.insert port chan)


-- | Runs node language. Runs interpreters for the underlying languages.
runNodeL :: NodeRuntime -> L.NodeL a -> IO a
runNodeL nodeRt = foldFree (interpretNodeL nodeRt)
