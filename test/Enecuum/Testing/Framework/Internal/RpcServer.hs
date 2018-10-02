module Enecuum.Testing.Framework.Internal.RpcServer where

import           Enecuum.Prelude

import qualified Data.Map as Map
import qualified Data.Aeson as A

import qualified Enecuum.Domain         as D
import qualified Enecuum.Language       as L

import qualified Enecuum.Testing.Types as T
import qualified Enecuum.Testing.RLens as RLens
import qualified Enecuum.Testing.Framework.Interpreters.Node as Impl

-- | Node RPC server worker.

startNodeRpcServer
  :: T.NodeRuntime
  -> p
  -> TVar (Map Text (A.Value -> Int -> L.NodeL D.RpcResponse))
  -> IO ()
startNodeRpcServer nodeRt _ methodVar = do
  methods <- readTVarIO methodVar
  control <- T.Control <$> newEmptyTMVarIO <*> newEmptyTMVarIO
  tId <- forkIO $ go 0 control methods

  let handle = T.RpcServerHandle tId control
  atomically $ putTMVar (nodeRt ^. RLens.rpcServer) handle

  where

    go iteration control methods = do
      act iteration control methods
      go (iteration + 1 :: Int) control methods

    act _ control methods = do
      controlReq <- atomically $ takeTMVar $ control ^. RLens.request
      case controlReq of
        T.RpcReq req -> do
          resp <- callRpc (Impl.runNodeL nodeRt) methods req
          atomically $ putTMVar (control ^. RLens.response) (T.AsRpcResp resp)
        _ -> error "Control request is not supported in RpcServer."

callRpc
  :: Monad m
  => (t -> m D.RpcResponse)
  -> Map Text (A.Value -> Int -> t)
  -> D.RpcRequest
  -> m D.RpcResponse
callRpc runner methods (D.RpcRequest method params reqId) =
    case method `Map.lookup` methods of
      Just justMethod -> runner $ justMethod params reqId
      Nothing -> return $ D.RpcResponseError
          (A.String $ "The method " <> method <> " isn't supported.")
          reqId
