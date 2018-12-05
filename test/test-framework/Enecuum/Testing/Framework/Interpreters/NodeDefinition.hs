module Enecuum.Testing.Framework.Interpreters.NodeDefinition where

import qualified Data.Map                                         as M
import           Enecuum.Prelude

import qualified Enecuum.Domain                                   as D
import qualified Enecuum.Framework.Lens                           as Lens
import qualified Enecuum.Language                                 as L

import qualified Enecuum.Testing.Core.Interpreters                as Impl
import qualified Enecuum.Testing.Framework.Interpreters.Node      as Impl
import qualified Enecuum.Testing.RLens                            as RLens
import qualified Enecuum.Testing.Types                            as T

import qualified Enecuum.Testing.Framework.Internal.RpcServer     as Impl (startNodeRpcServer)
import qualified Enecuum.Testing.Framework.Internal.TcpLikeServer as Impl (startNodeTcpLikeServer,
                                                                           stopNodeTcpLikeServer)

import qualified Enecuum.Framework.Handler.Network.Interpreter    as Impl
import qualified Enecuum.Framework.Handler.Rpc.Interpreter        as Impl (runRpcHandlerL)

addProcess :: T.NodeRuntime -> D.ProcessPtr a -> ThreadId -> IO ()
addProcess nodeRt pPtr threadId = do
    pId <- D.getProcessId pPtr
    ps <- atomically $ takeTMVar $ nodeRt ^. RLens.processes
    let newPs = M.insert pId threadId ps
    atomically $ putTMVar (nodeRt ^. RLens.processes) newPs

mkAddress :: T.NodeRuntime -> D.PortNumber -> D.Address
mkAddress nodeRt port = (nodeRt ^. RLens.address) & Lens.port .~ port

-- | Interpret NodeDefinitionL.
interpretNodeDefinitionL :: T.NodeRuntime -> L.NodeDefinitionF a -> IO a

interpretNodeDefinitionL nodeRt (L.SetNodeTag tag next) =
    next <$> atomically (writeTVar (nodeRt ^. RLens.tag) tag)

interpretNodeDefinitionL nodeRt (L.EvalNode nodeScript next) =
    next <$> Impl.runNodeL nodeRt nodeScript

interpretNodeDefinitionL nodeRt (L.ServingRpc port handlersF next) = do
    methodsMap <- atomically $ newTVar mempty
    Impl.runRpcHandlerL methodsMap handlersF
    Impl.startNodeRpcServer nodeRt port methodsMap
    pure $ next $ Just ()

interpretNodeDefinitionL nodeRt (L.ServingTcp port handlersF next) = do
    handlersMap <- atomically $ newTVar mempty
    Impl.runNetworkHandlerL handlersMap handlersF
    Impl.startNodeTcpLikeServer nodeRt (mkAddress nodeRt port) handlersMap
    pure $ next $ Just ()

interpretNodeDefinitionL nodeRt (L.StopServing port next) = do
    Impl.stopNodeTcpLikeServer nodeRt port
    pure $ next ()

interpretNodeDefinitionL nodeRt (L.ForkProcess action next) = do
    (pPtr, pVar) <- atomically (T.getNextId nodeRt) >>= D.createProcessPtr
    threadId <- forkIO $ do
        res <- Impl.runNodeL nodeRt action
        atomically $ putTMVar pVar res
    addProcess nodeRt pPtr threadId
    pure $ next pPtr

interpretNodeDefinitionL nodeRt (L.KillProcess _ _) = error "KillProcess not implemented."

interpretNodeDefinitionL _ (L.TryGetResult pPtr next) = do
    pVar <- D.getProcessVar pPtr
    mbResult <- atomically $ tryReadTMVar pVar
    pure $ next mbResult

interpretNodeDefinitionL _ (L.AwaitResult pPtr next) = do
    pVar <- D.getProcessVar pPtr
    result <- atomically $ takeTMVar pVar
    pure $ next result

interpretNodeDefinitionL _ L.ServingUdp{} = error "interpretNodeDefinitionL: ServingUdp not implemented"

interpretNodeDefinitionL _ (L.Std _ _) = error "STD not implemented in test runtime."

-- | Runs node definition language with node runtime.
runNodeDefinitionL :: T.NodeRuntime -> L.NodeDefinitionL a -> IO a
runNodeDefinitionL nodeRt = foldFree (interpretNodeDefinitionL nodeRt)
