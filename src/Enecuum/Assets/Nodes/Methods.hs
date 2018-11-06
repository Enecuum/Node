module Enecuum.Assets.Nodes.Methods where

import           Enecuum.Prelude
import qualified Enecuum.Domain                as D
import qualified Enecuum.Assets.Nodes.Messages as M
import qualified Enecuum.Language              as L
import           Enecuum.Research.ChordRouteMap

methodPing :: (L.Send con f, Functor f) => M.Ping -> con -> f ()
methodPing  M.Ping conn = void $ L.send conn M.Pong

rpcPingPong :: Applicative f => M.Ping -> f M.Pong
rpcPingPong M.Ping = pure M.Pong

handleStopNode
    :: L.HasStatus s (D.StateVar L.NodeStatus)
    => s -> M.Stop -> Free L.NodeF M.SuccessMsg
handleStopNode nodeData M.Stop = L.stopNode nodeData >> pure M.SuccessMsg


pingConnects :: ChordRouteMap D.Address -> L.NodeL [D.StringHash]
pingConnects nodes = do
    deadNodes <- forM (elems nodes) $ \(hash, D.Address host port) -> do
        res :: Either Text M.Pong <- L.makeRpcRequest (D.Address host (port - 1000)) M.Ping
        pure $ case res of Right _ -> Nothing ; Left _ -> Just hash
    pure $ catMaybes deadNodes
