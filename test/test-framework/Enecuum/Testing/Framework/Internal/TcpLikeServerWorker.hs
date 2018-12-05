module Enecuum.Testing.Framework.Internal.TcpLikeServerWorker where

import           Enecuum.Prelude

import qualified Data.Map as Map
import qualified Data.Aeson as A
import           Data.Aeson

import qualified Enecuum.Domain         as D
import qualified Enecuum.Language       as L

import qualified Enecuum.Testing.Types as T
import qualified Enecuum.Testing.RLens as RLens

import Enecuum.Testing.Framework.Internal.TcpLikeServerBinding (registerConnection)

-- | Node TCP-like binded server worker.
startNodeTcpLikeWorker
    :: (L.NodeL () -> IO ())
    -> T.NodeRuntime
    -> Map Text (L.NetworkHandler D.Tcp L.NodeL)
    -> Maybe (D.Connection D.Tcp)
    -> IO T.ConnectionWorkerHandle
startNodeTcpLikeWorker nodeLRunner nodeRt handlers mbBackConn = do

    control   <- T.Control <$> newEmptyTMVarIO <*> newEmptyTMVarIO
    tBackConn <- maybe newEmptyTMVarIO newTMVarIO mbBackConn

    tId       <- forkIO $ go 0 control tBackConn

    pure $ T.ConnectionWorkerHandle tId control nodeRt tBackConn
  where

    go iteration control tBackConn = do
        void $ act iteration control tBackConn
        go (iteration + 1 :: Int) control tBackConn

    act _ control tBackConn = do
        controlReq <- atomically $ takeTMVar $ control ^. RLens.request
        case controlReq of
            T.AcceptBackConnectionReq bindedServer -> do
                atomically $ putTMVar tBackConn (D.Connection (D.BoundAddress $ bindedServer ^. RLens.address) 0)
                registerConnection nodeRt bindedServer

            T.MessageReq msg -> do
                backConn <- atomically $ readTMVar tBackConn
                case decode msg of
                    Nothing  -> pure () -- TODO: error response here.
                    Just val -> callHandler nodeLRunner backConn handlers val
            _ -> error "Control request is not supported in binded Tcp-like server."
        atomically $ putTMVar (control ^. RLens.response) T.AsSuccessResp


callHandler
    :: (L.NodeL () -> IO ())
    -> D.Connection D.Tcp 
    -> Map Text (A.Value -> D.Connection D.Tcp -> L.NodeL ())
    -> D.NetworkMsg
    -> IO ()
callHandler nodeLRunner backConn handlers (D.NetworkMsg tag val) = case Map.lookup tag handlers of
    Nothing     -> pure () -- TODO: some error response here.
    Just method -> void $ forkIO $ nodeLRunner (method val backConn)


