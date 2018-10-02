module Enecuum.Testing.Framework.Internal.TcpLikeServer where

import           Enecuum.Prelude

import qualified Data.Map as Map
import qualified Data.Aeson as A

import qualified Enecuum.Domain         as D
import qualified Enecuum.Language       as L
import qualified Enecuum.Framework.Lens as Lens

import qualified Enecuum.Testing.Types as T
import qualified Enecuum.Testing.RLens as RLens
import qualified Enecuum.Testing.Framework.Interpreters.Node as Impl


bindServer :: T.NodeRuntime -> D.Address -> T.NodeRole -> T.ServerHandle -> IO T.BindedServer
bindServer nodeRt servingAddress role handle = do
    connections <- atomically $ takeTMVar $ nodeRt ^. RLens.connections
    let bindingAddress = servingAddress & Lens.port .~ fromIntegral (Map.size connections)
    let bindedServer = T.BindedServer bindingAddress handle
    let newConnection = T.NodeConnection role bindedServer
    let newConnections = Map.insert bindingAddress newConnection connections
    atomically $ putTMVar (nodeRt ^. RLens.connections) newConnections
    pure bindedServer

-- | Node TCP-like accepting server worker.
-- startNodeTcpLikeServer :: T.NodeRuntime -> D.Address -> methods -> IO ()
startNodeTcpLikeServer nodeRt servingAddress methodVar = do

  methods <- readTVarIO methodVar
  control <- T.Control <$> newEmptyTMVarIO <*> newEmptyTMVarIO
  tId <- forkIO $ go 0 control methods

  let handle = T.ServerHandle tId control nodeRt
  atomically $ do
    -- Own node's accepting servers
    servers <- takeTMVar (nodeRt ^. RLens.servers)
    putTMVar (nodeRt ^. RLens.servers) $ Map.insert servingAddress handle servers

    -- All servers
    serversRegistry <- takeTMVar (nodeRt ^. RLens.serversRegistry)
    putTMVar (nodeRt ^. RLens.serversRegistry) $ Map.insert servingAddress handle serversRegistry

  where

    go iteration control methods = do
      void $ act iteration control methods
      go (iteration + 1 :: Int) control methods

    act _ control methods = do
      controlReq <- atomically $ takeTMVar $ control ^. RLens.request
      controlResp <- case controlReq of
          T.EstablishConnectionReq -> do
              serverHandle <- startNodeTcpLikeWorker nodeRt methods
              bindedServer <- bindServer nodeRt servingAddress T.Server serverHandle
              pure $ T.AsConnectionAccepted bindedServer
          _ -> error $ "Control request is not supported in accepting Tcp-like server."
      atomically $ putTMVar (control ^. RLens.response) controlResp


-- | Node TCP-like binded server worker.
startNodeTcpLikeWorker nodeRt methods = do

  control <- T.Control <$> newEmptyTMVarIO <*> newEmptyTMVarIO
  tId <- forkIO $ go 0 control methods

  pure $ T.ServerHandle tId control nodeRt

  where

    go iteration control methods = do
      void $ act iteration control methods
      go (iteration + 1 :: Int) control methods

    act _ control methods = do
      controlReq <- atomically $ takeTMVar $ control ^. RLens.request
      controlResp <- case controlReq of
          _ -> error $ "Control request is not supported in binded Tcp-like server."
      atomically $ putTMVar (control ^. RLens.response) controlResp