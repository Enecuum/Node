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
import           Enecuum.Legacy.Refact.Network.Server
import           Enecuum.Framework.RpcMethod.Interpreter
import           Enecuum.Framework.Networking.Internal  as I
import           Enecuum.Framework.MsgHandler.Interpreter
import qualified Data.Map                                 as M

-- | Interpret NodeL.
interpretNodeL :: NodeRuntime -> L.NodeF a -> IO a
interpretNodeL nodeRt (L.EvalStateAtomically statefulAction next) =
    next <$> (atomically $ Impl.runStateL nodeRt statefulAction)

interpretNodeL _ (L.EvalGraphIO (L.GraphAction _ ioRunner act) next) =
  next <$> ioRunner act

interpretNodeL _ (L.EvalNetworking networking next) =
    next <$> Impl.runNetworkingL networking

interpretNodeL nodeRt (L.EvalCoreEffectNodeF coreEffects next) =
    next <$> Impl.runCoreEffect (nodeRt ^. RLens.coreRuntime) coreEffects

interpretNodeL nodeRt (L.StopNode next) = do
    atomically $ putTMVar (nodeRt ^. RLens.stopNode) True
    return $ next ()

interpretNodeL nodeRt (L.OpenConnection port initScript next) = do
    m <- atomically $ newTVar mempty
    a <- runMsgHandlerL m initScript
    handlers <- readTVarIO m
    next <$> I.openConnect port ((\f a b -> runNodeL nodeRt $ f a b) <$> handlers)

interpretNodeL _ (L.CloseConnection conn next) = do
    I.close conn
    return $ next ()

setServerChan :: TVar (Map PortNumber (TChan ServerComand)) -> PortNumber -> TChan ServerComand -> STM ()
setServerChan servs port chan = do
    serversMap <- readTVar servs
    whenJust (serversMap ^. at port) stopServer
    modifyTVar servs (M.insert port chan)



-- | Runs node language. Runs interpreters for the underlying languages.
runNodeL :: NodeRuntime -> L.NodeL a -> IO a
runNodeL nodeRt = foldFree (interpretNodeL nodeRt)
