module Enecuum.Framework.MsgHandler.Interpreter where

import           Enecuum.Prelude
import           Control.Monad.Free()

import qualified Data.Map as M
import           Enecuum.Framework.MsgHandler.Language

interpretMsgHandlerL :: TVar (M.Map Text (MsgHandler m)) -> MsgHandlerF m a -> IO a
interpretMsgHandlerL m (MsgHandler name method' next) = do
    atomically $ modifyTVar m (M.insert name method')
    pure (next ())

runMsgHandlerL :: TVar (Map Text (MsgHandler m)) -> MsgHandlerL m a -> IO a
runMsgHandlerL m = foldFree (interpretMsgHandlerL m)
