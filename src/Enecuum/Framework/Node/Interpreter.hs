module Enecuum.Framework.Node.Interpreter where

import Enecuum.Prelude

import           Eff                                ( handleRelay )

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L
import qualified Enecuum.Framework.Lens             as Lens

import Enecuum.Framework.Networking.Interpreter
import Enecuum.Framework.NetworkModel.Interpreter

{-
-- | Interpret NodeL. Does nothing ATM.
interpretNodeL
  :: L.NodeL a
  -> Eff '[L.NetworkingL, L.NetworkSyncL, L.NetworkListeningL, L.NetworkSendingL, L.LoggerL, SIO, Exc SomeException] a
interpretNodeL (L.Dummy) = L.logInfo "L.Dummy"

-- | Runs node model. Runs interpreters for the underlying languages.
runNodeModel
  :: Eff L.NodeModel a
  -> Eff '[L.LoggerL, SIO, Exc SomeException] a
runNodeModel
  = handleRelay pure ( (>>=) . interpretNetworkSendingL  )
  . handleRelay pure ( (>>=) . interpretNetworkListeningL  )
  . handleRelay pure ( (>>=) . interpretNetworkSyncL  )
  . handleRelay pure ( (>>=) . interpretNetworkingL  )
  . handleRelay pure ( (>>=) . interpretNodeL  )
  -}