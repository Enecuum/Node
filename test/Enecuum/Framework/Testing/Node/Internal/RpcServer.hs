module Enecuum.Framework.Testing.Node.Internal.RpcServer where

import           Enecuum.Prelude

import qualified Data.Map as Map

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L
import qualified Enecuum.Framework.Lens             as Lens

import           Enecuum.Core.Testing.Runtime.Types

import           Enecuum.Framework.Testing.Types
import qualified Enecuum.Framework.Testing.Lens as RLens

import           Enecuum.Core.Testing.Runtime.Logger.Impl

startNodeRpcServer
  :: NodeRuntime
  -> L.HandlersF
  -> IO ()
startNodeRpcServer nodeRt handlersF = do
  control <- NodeRpcServerControl <$> newEmptyTMVarIO <*> newEmptyTMVarIO
  tId <- forkIO $ go 0 control

  let handle = NodeRpcServerHandle tId control
  atomically $ putTMVar (nodeRt ^. RLens.rpcServer) handle

  where
    go :: Integer -> NodeRpcServerControl -> IO ()
    go iteration control = act iteration control >> go (iteration + 1) control

    act :: Integer -> NodeRpcServerControl -> IO ()
    act _ control = do
      req <- atomically $ takeTMVar $ control ^. RLens.request
      case req of
        RpcRequest rawData -> do
          -- let (handleProcessor, _) = handlersF (pure (), Just rawData)
          -- handle request here
          atomically $ putTMVar (control ^. RLens.response) Ack
          error "RpcRequest"
