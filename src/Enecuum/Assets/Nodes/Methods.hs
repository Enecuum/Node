module Enecuum.Assets.Nodes.Methods where

import qualified Enecuum.Assets.Nodes.Messages as M
import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import           Enecuum.Prelude


methodPing :: (L.Send con f, Functor f) => M.Ping -> con -> f ()
methodPing  M.Ping conn = void $ L.send conn M.Pong

rpcPingPong :: Applicative f => M.Ping -> f M.Pong
rpcPingPong M.Ping = pure M.Pong

handleStopNode
    :: L.HasStatus s (D.StateVar D.NodeStatus)
    => s -> M.Stop -> Free L.NodeF M.SuccessMsg
handleStopNode nodeData M.Stop = L.stopNode nodeData >> pure M.SuccessMsg

portError :: D.PortNumber -> Text -> Text
portError port protocol =
    "Port " <> show port <> " (for " <> protocol <> " server) is alredy used."
