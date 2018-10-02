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


bindConnection :: T.NodeRuntime -> D.Address -> T.ConnectionType -> IO T.BindingAddress
bindConnection nodeRt servingAddress connType = do
    connections <- atomically $ takeTMVar $ nodeRt ^. RLens.connections
    let bindingAddress = servingAddress & Lens.port .~ fromIntegral (Map.size connections)
    let newConnection = T.NodeConnection (nodeRt ^. RLens.address) bindingAddress connType
    let newConnections = Map.insert bindingAddress newConnection connections
    atomically $ putTMVar (nodeRt ^. RLens.connections) newConnections
    pure bindingAddress

-- | Node RPC server worker.
-- startNodeTcpLikeServer :: T.NodeRuntime -> D.Address -> methods -> IO ()
startNodeTcpLikeServer nodeRt servingAddress methodVar = do

  methods <- readTVarIO methodVar
  control <- T.Control <$> newEmptyTMVarIO <*> newEmptyTMVarIO
  tId <- forkIO $ go 0 control methods

  let handle = T.ServerHandle tId control nodeRt
  atomically $ do
    servers <- takeTMVar (nodeRt ^. RLens.servers)
    putTMVar (nodeRt ^. RLens.servers) $ Map.insert servingAddress handle servers

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
              bindingAddress <- bindConnection nodeRt servingAddress T.Server

              -- TODO: handle worker thread
              -- _ <- startNodeTcpLikeWorker nodeRt methods

              pure $ T.AsConnectionAccepted bindingAddress 
          _ -> error $ "Control request is not supported in TcpLikeServer."
      atomically $ putTMVar (control ^. RLens.response) controlResp