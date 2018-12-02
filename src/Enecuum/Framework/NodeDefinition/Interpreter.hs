{-# LANGUAGE PackageImports #-}

module Enecuum.Framework.NodeDefinition.Interpreter where

import           Data.Aeson                                       as A
import           Data.Aeson.Lens
import qualified Data.Map                                         as M
import qualified Data.Text                                        as T
import qualified "rocksdb-haskell" Database.RocksDB               as Rocks
import           Enecuum.Prelude                                  hiding (fromJust)
import qualified Network.Socket                                   as S hiding (recv)
import qualified Network.Socket.ByteString.Lazy                   as S
import           System.Console.Haskeline
import           System.Console.Haskeline.History

import qualified Enecuum.Core.Interpreters                        as Impl
import qualified Enecuum.Core.RLens                               as RLens
import qualified Enecuum.Core.Runtime                             as Impl (getNextId)
import qualified Enecuum.Core.Runtime                             as R
import qualified Enecuum.Framework.Domain.Networking              as D
import qualified Enecuum.Framework.Domain.Process                 as D
import qualified Enecuum.Framework.Domain.RPC                     as D
import           Enecuum.Framework.Handler.Cmd.Interpreter        as Cmd
import qualified Enecuum.Framework.Handler.Network.Interpreter    as Net
import           Enecuum.Framework.Handler.Rpc.Interpreter
import qualified Enecuum.Framework.Language                       as L
import qualified Enecuum.Framework.Networking.Internal.Connection as Conn
import           Enecuum.Framework.Networking.Internal.Tcp.Server
import           Enecuum.Framework.Node.Interpreter               (runNodeL)
import qualified Enecuum.Framework.Node.Interpreter               as Impl
import qualified Enecuum.Framework.RLens                          as RLens
import           Enecuum.Framework.Runtime                        (Connections, DBHandle, NodeRuntime)
import qualified Enecuum.Framework.Runtime                        as R


getNextId :: NodeRuntime -> IO Int
getNextId nodeRt = atomically $ Impl.getNextId $ nodeRt ^. RLens.coreRuntime . RLens.stateRuntime

addProcess :: NodeRuntime -> D.ProcessPtr a -> ThreadId -> IO ()
addProcess nodeRt pPtr threadId = do
    pId <- D.getProcessId pPtr
    ps <- readTVarIO $ nodeRt ^. RLens.processes
    let newPs = M.insert pId threadId ps
    atomically $ writeTVar (nodeRt ^. RLens.processes) newPs

popProcess :: NodeRuntime -> D.ProcessPtr a -> IO (Maybe ThreadId)
popProcess nodeRt pPtr = do
    pId <- D.getProcessId pPtr
    ps <- readTVarIO $ nodeRt ^. RLens.processes
    let mbThreadId = M.lookup pId ps
    let newPs = M.delete pId ps
    atomically $ writeTVar (nodeRt ^. RLens.processes) newPs
    pure mbThreadId

registerConnection
    :: R.AsNativeConnection protocol
    => R.ConnectionsVar protocol
    -> Conn.NativeConnection protocol
    -> IO ()
registerConnection connectionsVar nativeConn = do
    let conn = R.getConnection nativeConn
    connections <- atomically $ takeTMVar connectionsVar
    atomically $ putTMVar connectionsVar $ M.insert conn nativeConn connections

unregisterConnection
    :: R.ConnectionsVar protocol
    -> D.Connection protocol
    -> IO ()
unregisterConnection connectionsVar conn = do
    connections <- atomically $ takeTMVar connectionsVar
    atomically $ putTMVar connectionsVar $ M.delete conn connections

mkRegister :: R.ConnectionsVar protocol -> Conn.ConnectionRegister protocol
mkRegister connectionsVar = Conn.ConnectionRegister
    { Conn.addConnection    = registerConnection connectionsVar
    , Conn.removeConnection = unregisterConnection connectionsVar
    }

startServer
    :: Conn.NetworkConnection protocol
    => NodeRuntime
    -> R.ConnectionsVar protocol
    -> S.PortNumber
    -> L.NetworkHandlerL protocol L.NodeL a
    -> IO (Maybe ())
startServer nodeRt connectionsVar port handlersScript = do
    let logger = R.mkRuntimeLogger $ nodeRt ^. RLens.coreRuntime . RLens.loggerRuntime

    m        <- atomically $ newTVar mempty
    a        <- Net.runNetworkHandlerL m handlersScript
    handlers <- readTVarIO m

    let serversVar = nodeRt ^. RLens.servers
    servers <- atomically $ takeTMVar serversVar
    res <- if M.member port servers
        then pure Nothing
        else Conn.startServer
            logger
            (nodeRt ^. RLens.connectCounter)
            port
            ((\f a' b -> Impl.runNodeL nodeRt $ f a' b) <$> handlers)
            (mkRegister connectionsVar)
    atomically $ do
        whenJust res $ \servHandl ->
            putTMVar serversVar $ M.insert port servHandl servers
        unless (isJust res) $
            putTMVar serversVar servers
    pure $ if isJust res then Just () else Nothing

-- | Stop the server
stopServer :: R.ServerHandle -> IO ()
stopServer (R.ServerHandle sockVar acceptWorkerId) = do
    sock <- atomically $ takeTMVar sockVar
    Conn.closeConnection' sock acceptWorkerId
    atomically (putTMVar sockVar sock)

interpretNodeDefinitionL :: NodeRuntime -> L.NodeDefinitionF a -> IO a
interpretNodeDefinitionL nodeRt (L.SetNodeTag tag next) = do
    atomically $ writeTVar (nodeRt ^. RLens.nodeTag) tag
    pure $ next ()

interpretNodeDefinitionL nodeRt (L.EvalNode action next) = next <$> Impl.runNodeL nodeRt action

interpretNodeDefinitionL nodeRt (L.ServingTcp port action next) = do
    let tcpConnectionsVar = nodeRt ^. RLens.tcpConnects
    next <$> startServer nodeRt tcpConnectionsVar port action

interpretNodeDefinitionL nodeRt (L.ServingUdp port action next) = do
    let udpConnectionsVar = nodeRt ^. RLens.udpConnects
    next <$> startServer nodeRt udpConnectionsVar port action

interpretNodeDefinitionL nodeRt (L.StopServing port next) = do
    serversMap <- atomically $ takeTMVar (nodeRt ^. RLens.servers)
    whenJust (serversMap ^. at port) stopServer
    atomically $ putTMVar (nodeRt ^. RLens.servers) $ M.delete port serversMap
    pure $ next ()

interpretNodeDefinitionL nodeRt (L.GetBoundedPorts next) = do
    serversMap <- atomically $ readTMVar (nodeRt ^. RLens.servers)
    pure $ next $ M.keys serversMap

interpretNodeDefinitionL nodeRt (L.ServingRpc port action next) = do
    let logger = R.mkRuntimeLogger $ nodeRt ^. RLens.coreRuntime . RLens.loggerRuntime

    m <- atomically $ newTVar mempty
    a <- runRpcHandlerL m action
    handlerMap <- readTVarIO m

    let serversVar = nodeRt ^. RLens.servers
    servers <- atomically $ takeTMVar serversVar
    res <- if M.member port servers
        then pure Nothing
        else runRpcServer logger port (runNodeL nodeRt) handlerMap

    atomically $ do
        whenJust res $ \servHandl ->
            putTMVar serversVar $ M.insert port servHandl servers
        unless (isJust res) $
            putTMVar serversVar servers

    pure $ if isJust res then next $ Just () else next Nothing

interpretNodeDefinitionL nodeRt (L.Std handlers next) = do
    m <- atomically $ newTVar mempty
    _ <- runCmdHandlerL m handlers
    void $ forkIO $ do
        m'       <- readTVarIO m
        tag      <- readTVarIO (nodeRt ^. RLens.nodeTag)
        let
            filePath = nodeRt ^. RLens.storyPaths.at tag
            inpStr = if tag == "Client" then "λ> " else ""
            loop   = do
                minput <- getInputLine inpStr
                case minput of
                    Nothing      -> pure ()
                    Just    line -> do
                        res <- liftIO $ callHandler nodeRt m' line
                        outputStrLn $ T.unpack res
                        whenJust filePath $ \path -> do
                            history <- getHistory
                            liftIO $ writeHistory path history
                        loop

        runInputT defaultSettings{historyFile = filePath} loop
    pure $ next ()

-- TODO: make a separate language and use its interpreter in test runtime too.
interpretNodeDefinitionL nodeRt (L.ForkProcess action next) = do
    (pPtr, pVar) <- getNextId nodeRt >>= D.createProcessPtr
    threadId <- forkIO $ do
        res <- runNodeL nodeRt action
        atomically $ putTMVar pVar res
    addProcess nodeRt pPtr threadId
    pure $ next pPtr

interpretNodeDefinitionL nodeRt (L.KillProcess pId next) = do
    mbThreadId <- popProcess nodeRt pId
    whenJust mbThreadId killThread
    pure $ next ()

interpretNodeDefinitionL _ (L.TryGetResult pPtr next) = do
    pVar <- D.getProcessVar pPtr
    mbResult <- atomically $ tryReadTMVar pVar
    pure $ next mbResult

interpretNodeDefinitionL _ (L.AwaitResult pPtr next) = do
    pVar <- D.getProcessVar pPtr
    result <- atomically $ takeTMVar pVar
    pure $ next result

callHandler :: NodeRuntime -> Map Text (String -> L.NodeL Text) -> String -> IO Text
callHandler nodeRt methods msg = do
    let tag = T.pack $ takeWhile (/= ' ') msg
    case methods ^. at tag of
        Just justMethod -> Impl.runNodeL nodeRt $ justMethod msg
        Nothing         -> pure $ "The method " <> tag <> " isn't supported."

type RpcMethods t = Map Text (A.Value -> Int -> t)

runRpcServer
    :: R.RuntimeLogger
    -> S.PortNumber
    -> (t -> IO D.RpcResponse)
    -> RpcMethods t
    -> IO (Maybe R.ServerHandle)
runRpcServer logger port runner methods = runTCPServer logger port $ \sock -> do
    msg      <- S.recv sock (1024 * 4)
    response <- callRpc runner methods msg
    S.sendAll sock $ A.encode response

callRpc :: Monad m => (t -> m D.RpcResponse) -> RpcMethods t -> LByteString -> m D.RpcResponse
callRpc runner methods msg = case A.decode msg of
    Just (D.RpcRequest method params reqId) -> case method `M.lookup` methods of
        Just justMethod -> runner $ justMethod params reqId
        Nothing         -> pure $ D.RpcResponseError (A.String $ "The method " <> method <> " isn't supported.") reqId
    Nothing -> pure $ D.RpcResponseError (A.String "error of request parsing") 0

runNodeDefinitionL :: NodeRuntime -> Free L.NodeDefinitionF a -> IO a
runNodeDefinitionL nodeRt = foldFree (interpretNodeDefinitionL nodeRt)

-- TODO: move it somewhere.
-- TODO: FIXME: stop network workers
clearNodeRuntime :: NodeRuntime -> IO ()
clearNodeRuntime nodeRt = do
    serverPorts <- M.keys  <$> atomically (readTMVar (nodeRt ^. RLens.servers))
    processIds  <- M.elems <$> readTVarIO (nodeRt ^. RLens.processes)
    databases   <- M.elems <$> readTVarIO (nodeRt ^. RLens.databases)
    mapM_ (runNodeDefinitionL nodeRt . L.stopServing) serverPorts
    mapM_ killThread processIds
    mapM_ releaseDB databases

-- TODO: move it somewhere.
releaseDB :: DBHandle -> IO ()
releaseDB dbHandle = Rocks.close $ dbHandle ^. RLens.db
