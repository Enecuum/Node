module Enecuum.Framework.Domain.Process
    ( ProcessHandle
    , createProcessHandle
    ) where

import Enecuum.Prelude


type ProcessId = Int
data ProcessHandle a = ProcessHandle
    { processId :: ProcessId
    , threadId :: ThreadId
    }

createProcessHandle :: ThreadId -> ProcessId -> IO (ProcessHandle a)
createProcessHandle threadId processId = pure $ ProcessHandle processId threadId