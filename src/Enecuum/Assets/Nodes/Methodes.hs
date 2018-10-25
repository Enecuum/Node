module Enecuum.Assets.Nodes.Methodes where

import           Enecuum.Prelude
import qualified Enecuum.Domain                as D
import qualified Enecuum.Assets.Nodes.Messages as M
import qualified Enecuum.Language              as L

methodePing     M.Ping conn     = void $ L.send conn M.Pong
rpcPingPong     M.Ping          = pure M.Pong
methodeStopNode nodeData M.Stop = L.stopNode nodeData >> pure M.SuccessMsg