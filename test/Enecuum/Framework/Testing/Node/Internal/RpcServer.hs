module Enecuum.Framework.Testing.Node.Internal.RpcServer where

import           Enecuum.Prelude

import qualified Data.Map as Map
import qualified Data.Aeson as A

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L
import qualified Enecuum.Framework.Lens             as Lens

import           Enecuum.Core.Testing.Runtime.Types
import           Enecuum.Core.Testing.Runtime.Logger.Impl

import           Enecuum.Framework.Testing.Types
import qualified Enecuum.Framework.Testing.Lens as RLens

import           Enecuum.Framework.Testing.Node.Interpreters.NodeModel
import qualified Enecuum.Framework.Domain.RpcMessages as R

-- TODO: consider to use forever.
-- | Node RPC server worker.

startNodeRpcServer nodeRt methodVar = do
  methods <- readTVarIO methodVar
  control <- Control <$> newEmptyTMVarIO <*> newEmptyTMVarIO
  tId <- forkIO $ go 0 control methods

  let handle = RpcServerHandle tId control
  atomically $ putTMVar (nodeRt ^. RLens.rpcServer) handle

  where

    go iteration control methods = do
      act iteration control methods
      go (iteration + 1) control methods

    act _ control methods = do
      req <- atomically $ takeTMVar $ control ^. RLens.request
      case req of
        RpcRequest req -> do
          resp <- callRpc (runNodeModel nodeRt) methods req
          atomically $ putTMVar (control ^. RLens.response) (AsRpcResponse resp)
        _                                   -> error "Unknown ControlRequest."

callRpc runner methods (R.RpcRequest method params reqId) =
    case method `Map.lookup` methods of
      Just justMethod -> runner $ justMethod params reqId
      Nothing -> return $ R.RpcResponseError
          (A.String $ "The method " <> method <> " is'nt supported.")
          reqId

