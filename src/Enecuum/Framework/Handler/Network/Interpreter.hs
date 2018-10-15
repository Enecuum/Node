module Enecuum.Framework.Handler.Network.Interpreter where

import           Enecuum.Prelude
import           Control.Monad.Free()

import qualified Data.Map as M
import           Enecuum.Framework.Handler.Network.Language

interpretNetworkHandlerL :: TVar (M.Map Text (NetworkHandler p m)) -> NetworkHandlerF p m a -> IO a
interpretNetworkHandlerL m (NetworkHandler name method' next) = do
    atomically $ modifyTVar m (M.insert name method')
    pure (next ())

runNetworkHandlerL :: TVar (Map Text (NetworkHandler p m)) -> NetworkHandlerL p m a -> IO a
runNetworkHandlerL m = foldFree (interpretNetworkHandlerL m)
