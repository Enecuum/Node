module Enecuum.Samples.Assets.Nodes.Methods where

import qualified Enecuum.Samples.Assets.Nodes.Messages as M
import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import           Enecuum.Prelude


methodPing :: (L.Send con f, Functor f) => D.Ping -> con -> f ()
methodPing  D.Ping conn = void $ L.send conn D.Pong

rpcPingPong :: Applicative f => D.Ping -> f D.Pong
rpcPingPong D.Ping = pure D.Pong

handleStopNode
    :: L.HasStatus s (D.StateVar D.NodeStatus)
    => s -> D.Stop -> Free L.NodeF D.SuccessMsg
handleStopNode nodeData D.Stop = L.stopNode nodeData >> pure D.SuccessMsg

portError :: D.PortNumber -> Text -> Text
portError port protocol =
    "Port " <> show port <> " (for " <> protocol <> " server) is alredy used."
