module Enecuum.Framework.Testing.Node.Internal.RpcServer where

import           Enecuum.Prelude

import qualified Data.Map as Map

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L
import qualified Enecuum.Framework.Lens             as Lens

import           Enecuum.Core.Testing.Runtime.Types
import           Enecuum.Core.Testing.Runtime.Logger.Impl

import           Enecuum.Framework.Testing.Types
import qualified Enecuum.Framework.Testing.Lens as RLens

import           Enecuum.Framework.Testing.Node.Interpreters.NodeModel

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
        ControlRpcRequest (D.RpcRequest rawDataIn) -> do
          let (handler, _) = handlersF (pure Nothing, rawDataIn)
          mbResult <- runSafeIO
              $ runLoggerL (nodeRt ^. RLens.loggerRuntime)
              $ runNodeModel nodeRt handler
          case mbResult of
            Nothing -> atomically $ putTMVar (control ^. RLens.response)
                                  $ ControlRpcResponse
                                  $ D.RpcResponse "errror"
            Just rawDataOut -> atomically $ putTMVar (control ^. RLens.response)
                                  $ ControlRpcResponse
                                  $ D.RpcResponse rawDataOut
