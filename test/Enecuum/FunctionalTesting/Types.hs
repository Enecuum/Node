{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Testing.Types where

import           Enecuum.Prelude

import qualified Data.Map                             as Map

import qualified Enecuum.Domain                       as D

-- Bad dependency of Testing from TestData.
import qualified Enecuum.TestData.TestGraph as TG

-- | Defines control requests to manipulate by nodes.
data ControlRequest
  = RpcRequest RpcRequest       -- ^ Eval RPC request to a node.
  | RelayRpcRequest             -- ^ Relay RPC request from one node to another.
    { _from    :: D.Address     -- ^ From node
    , _to      :: D.Address     -- ^ To node
    , _request :: RpcRequest    -- ^ RPC request to relay
    }

-- | Result of evaluation of control response.
data ControlResponse
  = AsRpcResponse RpcResponse   -- ^ RPC response wrapped into the control response.
  | AsErrorResponse Text          -- ^ Keeps an error that occured during the ControlRequest evaluation.

-- | Control is the way to evaluate admin control over a node.
-- Represents the MVar Reqeust-Response pattern (STM version).
data Control = Control
  { _request  :: TMVar ControlRequest   -- ^ ControlRequest channel.
  , _response :: TMVar ControlResponse  -- ^ ControlResponse channel.
  }

-- | RPC Server handle.
data RpcServerHandle = RpcServerHandle
  { _threadId :: ThreadId   -- ^ Server thread ID.
  , _control  :: Control    -- ^ Server control interface.
  }

-- | Logger runtime. Stores messages.
data LoggerRuntime = LoggerRuntime
  { _messages     :: TVar [Text]
  }

data VarHandle = VarHandle D.VarId (TVar Any)
type NodeState = TMVar (Map.Map D.VarId VarHandle)

-- | Test runtime for every node acting within a particular test runtime.
data NodeRuntime = NodeRuntime
  { _loggerRuntime  :: LoggerRuntime    -- ^ Logger runtime.
  , _networkControl :: Control                -- ^ Control interface for virtual network.
  , _address        :: D.Address          -- ^ Address of this node.
  , _tag            :: TVar D.NodeTag         -- ^ Tag of this node.
  , _rpcServer      :: TMVar RpcServerHandle  -- ^ RPC server of this node.
  , _graph          :: TG.TestGraphVar        -- ^ Graph
  , _varCounter     :: TMVar Int              -- ^ Vars counter. Used to generate VarId.
  , _state          :: NodeState              -- ^ State of node.
  }

-- | Registry of nodes acting within a test network.
type NodesRegistry = TMVar (Map.Map D.Address NodeRuntime)

-- | Test runtime describing a virtual network and nodes acting within it.
-- In the testing runtime, the network environment is represented
-- as a separate thread. The network environment can relay RPC requests
-- from one node to another, it can make RPC requests to nodes,
-- and can evaluate some control requests over the test network.
data TestRuntime = TestRuntime
  { _loggerRuntime   :: LoggerRuntime -- ^ Logger runtime.
  , _networkWorkerId :: ThreadId            -- ^ Network environment thread ID.
  , _networkControl  :: Control             -- ^ Network environment control interface
  , _registry        :: NodesRegistry       -- ^ Tag of this node.
  }
