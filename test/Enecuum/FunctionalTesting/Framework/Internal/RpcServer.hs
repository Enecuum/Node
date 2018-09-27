module Enecuum.FunctionalTesting.Framework.Internal.RpcServer where

import           Enecuum.Prelude

import qualified Data.Map as Map
import qualified Data.Aeson as A

import qualified Enecuum.Domain         as D
import qualified Enecuum.Language       as L
import qualified Enecuum.Framework.Lens as Lens

import qualified Enecuum.FunctionalTesting.Types as T
import qualified Enecuum.FunctionalTesting.RLens as RLens
import qualified Enecuum.FunctionalTesting.Framework.Interpreters.Node as Impl

-- | Node RPC server worker.

startNodeRpcServer nodeRt port methodVar = do
  methods <- readTVarIO methodVar
  control <- Control <$> newEmptyTMVarIO <*> newEmptyTMVarIO
  tId <- forkIO $ go 0 control methods

  let handle = RpcServerHandle tId control
  atomically $ putTMVar ( nodeRt ^. RLens.rpcServer) handle

  where

    go iteration control methods = do
      act iteration control methods
      go (iteration + 1) control methods

    act _ control methods = do
      req <- atomically $ takeTMVar $ control ^. RLens.request
      case req of
        RpcRequest req -> do
          resp <- callRpc (Impl.runNodeL nodeRt) methods req
          atomically $ putTMVar (control ^. RLens.response) (AsRpcResponse resp)
        _                                   -> error "Unknown ControlRequest."

callRpc
  :: Monad m
  => (t -> m R.RpcResponse)
  -> Map Text (A.Value -> Int -> t)
  -> R.RpcRequest
  -> m R.RpcResponse
callRpc runner methods (R.RpcRequest method params reqId) =
    case method `Map.lookup` methods of
      Just justMethod -> runner $ justMethod params reqId
      Nothing -> return $ R.RpcResponseError
          (A.String $ "The method " <> method <> " isn't supported.")
          reqId
