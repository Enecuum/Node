module Enecuum.Testing.Framework.Internal.TcpLikeServer where

import           Enecuum.Prelude

import qualified Data.Map as Map
import qualified Data.Aeson as A
import           Data.Aeson
import qualified Data.Aeson.Lens as ALens
import           Control.Concurrent (killThread)

import qualified Enecuum.Domain         as D
import qualified Enecuum.Language       as L
import qualified Enecuum.Framework.Lens as Lens

import qualified Enecuum.Testing.Types as T
import qualified Enecuum.Testing.RLens as RLens
import qualified Enecuum.Testing.Framework.Interpreters.Node as Impl
import           Enecuum.Testing.Framework.Internal.TcpLikeServerWorker (startNodeTcpLikeWorker)
import           Enecuum.Testing.Framework.Internal.TcpLikeServerBinding (bindServer)

-- | Node TCP-like accepting server worker.
startNodeTcpLikeServer :: T.NodeRuntime -> D.Address -> TVar (Map Text (L.MsgHandler L.NodeL)) -> IO ()
startNodeTcpLikeServer nodeRt servingAddress handlersVar = do

  handlers <- readTVarIO handlersVar
  control <- T.Control <$> newEmptyTMVarIO <*> newEmptyTMVarIO
  tId <- forkIO $ go 0 control handlers

  let handle = T.ServerHandle tId control nodeRt
  atomically $ do
    serversRegistry <- takeTMVar (nodeRt ^. RLens.serversRegistry)
    putTMVar (nodeRt ^. RLens.serversRegistry) $ Map.insert servingAddress handle serversRegistry

  where

    go iteration control handlers = do
      void $ act iteration control handlers
      go (iteration + 1 :: Int) control handlers

    act _ control handlers = do
      controlReq <- atomically $ takeTMVar $ control ^. RLens.request
      controlResp <- case controlReq of
          T.EstablishConnectionReq -> do
              workerHandle <- startNodeTcpLikeWorker (Impl.runNodeL nodeRt) nodeRt handlers Nothing
              bindedServer <- bindServer nodeRt (servingAddress ^. Lens.host) T.Server workerHandle
              pure $ T.AsConnectionAccepted bindedServer
          _ -> error "Control request is not supported in accepting Tcp-like server."
      atomically $ putTMVar (control ^. RLens.response) controlResp



-- | Node TCP-like accepting server worker.
stopNodeTcpLikeServer :: T.NodeRuntime -> D.PortNumber -> IO ()
stopNodeTcpLikeServer nodeRt port = do
    let servingAddress = (nodeRt ^. RLens.address) & Lens.port .~ port
    serversRegistry <- atomically $ takeTMVar (nodeRt ^. RLens.serversRegistry)
    case Map.lookup servingAddress serversRegistry of
      Nothing -> pure ()
      Just serverHandle -> killThread $ serverHandle ^. RLens.threadId
    atomically $ putTMVar (nodeRt ^. RLens.serversRegistry) $ Map.delete servingAddress serversRegistry
