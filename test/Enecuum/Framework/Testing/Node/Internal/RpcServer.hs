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

-- TODO: consider to use forever.
-- | Node RPC server worker.
startNodeRpcServer
  :: NodeRuntime
  -> L.HandlersF
  -> IO ()
startNodeRpcServer nodeRt handlersF = do
  control <- Control <$> newEmptyTMVarIO <*> newEmptyTMVarIO
  tId <- forkIO $ go 0 control

  let handle = RpcServerHandle tId control
  atomically $ putTMVar (nodeRt ^. RLens.rpcServer) handle

  where
    go :: Integer -> Control -> IO ()
    go iteration control = act iteration control >> go (iteration + 1) control

    act :: Integer -> Control -> IO ()
    act _ control = do
      req <- atomically $ takeTMVar $ control ^. RLens.request
      case req of
        RpcRequest (D.RpcRequest rawDataIn) -> makeRpcRequest control rawDataIn
        _                                   -> error "Unknown ControlRequest."
    
    makeRpcRequest control rawDataIn = do
      let (handler, _) = handlersF (pure Nothing, rawDataIn)
      mbResult <- runNodeModel nodeRt handler
      let resp = case mbResult of
            Nothing         -> AsErrorResponse $ "Method is not supported: " +|| rawDataIn ||+ ""
            Just rawDataOut -> AsRpcResponse   $ D.RpcResponse rawDataOut
      atomically $ putTMVar (control ^. RLens.response) resp
