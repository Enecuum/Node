module Enecuum.Framework.Handler.Udp.Interpreter where

import           Enecuum.Prelude
import           Control.Monad.Free()

import qualified Data.Map as M
import           Enecuum.Framework.Handler.Udp.Language

interpretUdpHandlerL :: TVar (M.Map Text (UdpHandler m)) -> UdpHandlerF m a -> IO a
interpretUdpHandlerL m (UdpHandler name method' next) = do
    atomically $ modifyTVar m (M.insert name method')
    pure (next ())

runUdpHandlerL :: TVar (Map Text (UdpHandler m)) -> UdpHandlerL m a -> IO a
runUdpHandlerL m = foldFree (interpretUdpHandlerL m)