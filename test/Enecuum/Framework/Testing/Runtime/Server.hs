module Enecuum.Framework.Testing.Runtime.Server where

import           Enecuum.Prelude

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L
import qualified Enecuum.Framework.Lens             as Lens

import           Enecuum.Framework.Testing.Runtime.Types
import           Enecuum.Framework.Testing.Runtime.STM
import           Enecuum.Framework.Testing.Runtime.NodeModel.Impl
import qualified Enecuum.Framework.Testing.Runtime.Lens as RLens

startNodeRpcServer
  :: NodeRuntime
  -> L.HandlersF
  -> IO ()
startNodeRpcServer rt handlersF = do
  control <- NodeRpcServerControl <$> newEmptyTMVarIO <*> newEmptyTMVarIO
  tId <- forkIO $ go 0 control

  let handle = NodeRpcServerHandle tId control
  atomically $ putTMVar (rt ^. RLens.rpcServer) handle

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
      

