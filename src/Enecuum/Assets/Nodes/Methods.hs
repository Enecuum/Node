module Enecuum.Assets.Nodes.Methods where

import           Enecuum.Prelude
import qualified Enecuum.Domain                as D
import qualified Enecuum.Assets.Nodes.Messages as M
import qualified Enecuum.Language              as L

methodPing  M.Ping conn = void $ L.send conn M.Pong
rpcPingPong M.Ping = pure M.Pong
methodStopNode nodeData M.Stop = L.stopNode nodeData >> pure M.SuccessMsg

