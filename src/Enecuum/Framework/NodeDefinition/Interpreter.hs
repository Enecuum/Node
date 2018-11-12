{-# LANGUAGE PackageImports #-}

module Enecuum.Framework.NodeDefinition.Interpreter where

import Enecuum.Prelude hiding (fromJust)
import qualified Data.Map                           as M
import           Data.Aeson                         as A
import           Control.Concurrent.STM.TChan
import           Control.Concurrent                 (killThread)
import qualified "rocksdb-haskell" Database.RocksDB as Rocks
import qualified Network.Socket.ByteString.Lazy     as S
import qualified Network.Socket                     as S hiding (recv)
import           Enecuum.Framework.Networking.Internal.Tcp.Server
import           Enecuum.Framework.Node.Interpreter        (runNodeL, setServerChan)
import           Enecuum.Framework.Runtime                 (NodeRuntime, DBHandle, Connections, getNextId)
import qualified Enecuum.Framework.Language                as L
import qualified Enecuum.Framework.RLens                   as RLens
import qualified Enecuum.Core.Interpreters                 as Impl
import qualified Enecuum.Framework.Node.Interpreter        as Impl
import qualified Enecuum.Framework.Domain.RPC              as D
import qualified Enecuum.Framework.Domain.Networking       as D
import qualified Enecuum.Framework.Domain.Process          as D
import qualified Enecuum.Framework.Runtime                        as R
import           Enecuum.Framework.Handler.Rpc.Interpreter
import qualified Enecuum.Framework.Handler.Network.Interpreter    as Net
import qualified Enecuum.Framework.Networking.Internal.Connection as Con
import           Enecuum.Framework.Handler.Cmd.Interpreter as Cmd
import           Data.Aeson.Lens
import qualified Data.Text as T
import           System.Console.Haskeline
import           System.Console.Haskeline.History
import           Enecuum.Framework.Networking.Internal.Connection (ServerHandle (..))

addProcess :: NodeRuntime -> D.ProcessPtr a -> ThreadId -> IO ()
addProcess nodeRt pPtr threadId = do
    pId <- D.getProcessId pPtr
    ps <- readTVarIO $ nodeRt ^. RLens.processes
    let newPs = M.insert pId threadId ps
    atomically $ writeTVar (nodeRt ^. RLens.processes) newPs


registerConnection
    :: D.AsNativeConnection protocol
    => Con.NetworkConnection protocol
    => R.ConnectionsVar protocol
    -> D.Connection protocol
    -> D.NativeConnection protocol
    -> IO Bool
registerConnection connectionsVar conn nativeConn = do
    trace_ "[registerConnection] taking connections"
    connections <- atomically $ takeTMVar connectionsVar

    trace_ "[registerConnection] checking prev connection"
    oldIsDead <- case conn `M.lookup` connections of
        Just nativeConn' -> do
            -- This probably should not happen because bound address won't be repeating.
            -- But when it is, it means we haven't closed prev connection and haven't unregistered it.
            -- TODO: check isClosed in the appropriate places.
            isDead <- D.isClosed nativeConn'
            when   isDead $ trace_ "[registerConnection] prev connection is dead, replacing by new"
            unless isDead $ trace_ "[registerConnection] prev connection is alive"
            pure isDead
        Nothing   -> do
            trace_ "[registerConnection] no prev connections found, inserting new"
            pure True

    let newConnections = M.insert conn nativeConn connections
    atomically
        $ putTMVar connectionsVar
        $ if oldIsDead then newConnections else connections
    pure oldIsDead

-- TODO: rework this
startServing
    :: (D.AsNativeConnection protocol, Con.NetworkConnection protocol)
    => NodeRuntime
    -> R.ConnectionsVar protocol
    -> S.PortNumber
    -> L.NetworkHandlerL protocol L.NodeL a
    -> IO a
startServing nodeRt connectionsVar port handlersScript = do
    m        <- atomically $ newTVar mempty
    a        <- Net.runNetworkHandlerL m handlersScript
    handlers <- readTVarIO m

    s <- Con.startServer
        port
        ((\f a' b -> Impl.runNodeL nodeRt $ f a' b) <$> handlers)
        (registerConnection connectionsVar)
    pure a


interpretNodeDefinitionL :: NodeRuntime -> L.NodeDefinitionF a -> IO a
interpretNodeDefinitionL nodeRt (L.NodeTag tag next) = do
    atomically $ writeTVar (nodeRt ^. RLens.nodeTag) tag
    pure $ next ()

interpretNodeDefinitionL nodeRt (L.EvalNodeL action next) = next <$> Impl.runNodeL nodeRt action

interpretNodeDefinitionL nodeRt (L.EvalCoreEffectNodeDefinitionF coreEffect next) =
    next <$> Impl.runCoreEffect (nodeRt ^. RLens.coreRuntime) coreEffect

interpretNodeDefinitionL nodeRt (L.ServingTcp port action next) = do
    let tcpConnectionsVar = nodeRt ^. RLens.tcpConnects
    next <$> startServing nodeRt tcpConnectionsVar port action

interpretNodeDefinitionL nodeRt (L.ServingUdp port action next) = do
    let udpConnectionsVar = nodeRt ^. RLens.udpConnects
    next <$> startServing nodeRt udpConnectionsVar port action

interpretNodeDefinitionL nodeRt (L.StopServing port next) = do
    -- Why server is not deleted from the map?
    serversMap <- readTVarIO (nodeRt ^. RLens.servers)
    whenJust (serversMap ^. at port) $ Con.stopServer
    pure $ next ()

interpretNodeDefinitionL nodeRt (L.ServingRpc port action next) = do
    m <- atomically $ newTVar mempty
    a <- runRpcHandlerL m action
    s <- takeServerChan (nodeRt ^. RLens.servers) port
    void $ forkIO $ runRpcServer s port (runNodeL nodeRt) m
    pure $ next a

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
                        res <- liftIO $ callHandler nodeRt m' $ T.pack line
                        outputStrLn $ T.unpack res
                        whenJust filePath $ \path -> do
                            history <- getHistory
                            liftIO $ writeHistory path history
                        loop

        runInputT defaultSettings{historyFile = filePath} loop
    pure $ next ()

-- TODO: make a separate language and use its interpreter in test runtime too.
interpretNodeDefinitionL nodeRt (L.ForkProcess action next) = do
    (pPtr, pVar) <- atomically (getNextId nodeRt) >>= D.createProcessPtr
    threadId <- forkIO $ do
        res <- runNodeL nodeRt action
        atomically $ putTMVar pVar res
    addProcess nodeRt pPtr threadId
    pure $ next pPtr

interpretNodeDefinitionL _ (L.TryGetResult pPtr next) = do
    pVar <- D.getProcessVar pPtr
    mbResult <- atomically $ tryReadTMVar pVar
    pure $ next mbResult

interpretNodeDefinitionL _ (L.AwaitResult pPtr next) = do
    pVar <- D.getProcessVar pPtr
    result <- atomically $ takeTMVar pVar
    pure $ next result

callHandler :: NodeRuntime -> Map Text (Value -> L.NodeL Text) -> Text -> IO Text
callHandler nodeRt methods msg = do
    val <- try $ pure $ A.decode $ fromString $ T.unpack msg
    case val of
        Right (Just jval@((^? key "method" . _String) -> Just method)) ->
            case methods ^. at method of
                Just justMethod -> Impl.runNodeL nodeRt $ justMethod jval
                Nothing         -> pure $ "The method " <> method <> " isn't supported."
        Right _                    -> pure "Error of request parsing."
        Left  (_ :: SomeException) -> pure "Error of request parsing."

-- TODO: treadDelay if server in port exist!!!
takeServerChan :: TVar (Map S.PortNumber ServerHandle) -> S.PortNumber -> IO ServerHandle
takeServerChan servs port = do
    chan <- atomically $ newTChan
    Impl.setServerChan servs port chan
    pure $ OldServerHandle chan


runRpcServer
    :: ServerHandle -> S.PortNumber -> (t -> IO D.RpcResponse) -> TVar (Map Text (A.Value -> Int -> t)) -> IO ()
runRpcServer (OldServerHandle chan) port runner methodVar = do
    methods <- readTVarIO methodVar
    runTCPServer chan port $ \sock -> do
        msg      <- S.recv sock (1024 * 4)
        response <- callRpc runner methods msg
        S.sendAll sock $ A.encode response

callRpc :: Monad m => (t -> m D.RpcResponse) -> Map Text (A.Value -> Int -> t) -> LByteString -> m D.RpcResponse
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
    serverPorts <- M.keys  <$> readTVarIO (nodeRt ^. RLens.servers  )
    threadIds   <- M.elems <$> readTVarIO (nodeRt ^. RLens.processes)
    databases   <- M.elems <$> readTVarIO (nodeRt ^. RLens.databases)
    mapM_ (runNodeDefinitionL nodeRt . L.stopServing) serverPorts
    mapM_ killThread threadIds
    mapM_ releaseDB databases

-- TODO: move it somewhere.
releaseDB :: DBHandle -> IO ()
releaseDB dbHandle = Rocks.close $ dbHandle ^. RLens.db
