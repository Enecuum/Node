module Enecuum.Testing.Framework.Internal.TcpLikeServerBinding where

import           Enecuum.Prelude

import qualified Data.Map as Map
import           Control.Concurrent (killThread)

import qualified Enecuum.Domain         as D
import qualified Enecuum.Framework.Lens as Lens

import qualified Enecuum.Testing.Types as T
import qualified Enecuum.Testing.RLens as RLens



-- | Register connection in the node connections list.
-- TODO: check if connection exists.
registerConnection :: T.NodeRuntime -> T.BindedServer -> IO ()
registerConnection nodeRt bindedServer = atomically $ do
    connections <- takeTMVar (nodeRt ^. RLens.connections)
    let newConnection  = T.NodeConnection T.Server bindedServer
    let newConnections = Map.insert (bindedServer ^. RLens.address) newConnection connections
    putTMVar (nodeRt ^. RLens.connections) newConnections

-- | Remove connection
removeConnection :: T.NodeRuntime -> D.TcpConnection -> IO (Maybe T.BindedServer)
removeConnection nodeRt connection = atomically $ do
    connections                  <- takeTMVar (nodeRt ^. RLens.connections)
    (mbNodeConn, newConnections) <- case Map.lookup (connection ^. Lens.address) connections of
        Nothing             -> pure (Nothing, connections)
        Just nodeConnection -> do
            let newConnections = Map.delete (connection ^. Lens.address) connections
            pure (Just nodeConnection, newConnections)
    putTMVar (nodeRt ^. RLens.connections) newConnections
    pure $ mbNodeConn >>= Just . (^. RLens.bindedServer)


bindServer :: T.NodeRuntime -> D.Host -> T.NodeRole -> T.ConnectionWorkerHandle -> IO T.BindedServer
bindServer nodeRt host role workerHandle = do
    connections <- atomically $ takeTMVar $ nodeRt ^. RLens.connections
    let bindingAddress = D.Address host (fromIntegral (Map.size connections))
    let bindedServer   = T.BindedServer bindingAddress workerHandle
    let newConnection  = T.NodeConnection role bindedServer
    let newConnections = Map.insert bindingAddress newConnection connections
    atomically $ putTMVar (nodeRt ^. RLens.connections) newConnections
    pure bindedServer


-- TODO: this is probably wrong, because we have to wait all STM operations first.
stopBindedServer :: T.BindedServer -> IO ()
stopBindedServer bindedServer = killThread $ bindedServer ^. RLens.handle . RLens.threadId

-- | Establish connection with the server through test environment.
-- TODO: check if connection exists.
closeConnection :: T.NodeRuntime -> D.TcpConnection -> IO ()
closeConnection nodeRt connection = removeConnection nodeRt connection >>= \case
    Nothing           -> pure ()
    Just bindedServer -> stopBindedServer bindedServer
