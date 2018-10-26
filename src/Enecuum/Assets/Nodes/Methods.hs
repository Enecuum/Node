module Enecuum.Assets.Nodes.Methods where

import           Enecuum.Prelude
import qualified Enecuum.Domain                as D
import qualified Enecuum.Assets.Nodes.Messages as M
import qualified Enecuum.Language              as L

methodPing :: (L.Send con f, Functor f) => M.Ping -> con -> f ()
methodPing  M.Ping conn = void $ L.send conn M.Pong

rpcPingPong :: Applicative f => M.Ping -> f M.Pong
rpcPingPong M.Ping = pure M.Pong

handleStopNode
    :: L.HasStatus s (D.StateVar L.NodeStatus)
    => s -> M.Stop -> Free L.NodeF M.SuccessMsg
handleStopNode nodeData M.Stop = L.stopNode nodeData >> pure M.SuccessMsg

