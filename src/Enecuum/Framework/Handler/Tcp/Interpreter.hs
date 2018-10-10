module Enecuum.Framework.Handler.Tcp.Interpreter where

import           Enecuum.Prelude
import           Control.Monad.Free()

import qualified Data.Map as M
import           Enecuum.Framework.Handler.Tcp.Language

interpretTcpHandlerL :: TVar (M.Map Text (TcpHandler m)) -> TcpHandlerF m a -> IO a
interpretTcpHandlerL m (TcpHandler name method' next) = do
    atomically $ modifyTVar m (M.insert name method')
    pure (next ())

runTcpHandlerL :: TVar (Map Text (TcpHandler m)) -> TcpHandlerL m a -> IO a
runTcpHandlerL m = foldFree (interpretTcpHandlerL m)
