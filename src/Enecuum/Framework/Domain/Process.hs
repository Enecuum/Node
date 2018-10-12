module Enecuum.Framework.Domain.Process
    ( ProcessId
    , ProcessPtr
    , ProcessVar
    , createProcessPtr
    , getProcessId
    , getProcessVar
    ) where

import Enecuum.Prelude

type ProcessVar a = TMVar a
type ProcessId = Int
data ProcessPtr a = ProcessPtr ProcessId (ProcessVar a)

createProcessPtr :: ProcessId -> IO (ProcessPtr a, ProcessVar a)
createProcessPtr pId = do
    pVar <- newEmptyTMVarIO
    pure (ProcessPtr pId pVar, pVar)

getProcessId :: ProcessPtr a -> IO ProcessId
getProcessId (ProcessPtr pId _) = pure pId

getProcessVar :: ProcessPtr a -> IO (ProcessVar a)
getProcessVar (ProcessPtr _ pVar) = pure pVar