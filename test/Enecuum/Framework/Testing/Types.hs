{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Framework.Testing.Types where

import Enecuum.Prelude

import qualified Data.Map as Map

import qualified Enecuum.Domain                       as D
import           Enecuum.Core.Testing.Runtime.Types
import qualified Enecuum.Framework.TestData.TestGraph as TG
import qualified Enecuum.Framework.Domain.Types       as T

-- | Defines control requests to manipulate by nodes.
data ControlRequest
  = RpcRequest D.RpcRequest       -- ^ Eval RPC request to a node.
  | RelayRpcRequest               -- ^ Relay RPC request from one node to another.
    { _from    :: D.NodeAddress   -- ^ From node
    , _to      :: D.NodeAddress   -- ^ To node
    , _request :: D.RpcRequest    -- ^ RPC request to relay
    }

-- | Result of evaluation of control response.
data ControlResponse
  = AsRpcResponse D.RpcResponse   -- ^ RPC response wrapped into the control response.
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

data VarHandle = VarHandle D.VarId (TVar Any)
type NodeState = TMVar (Map.Map D.VarId VarHandle)

-- | Test runtime for every node acting within a particular test runtime.
data NodeRuntime = NodeRuntime
  { _loggerRuntime  :: LoggerRuntime          -- ^ Logger runtime.
  , _networkControl :: Control                -- ^ Control interface for virtual network.
  , _address        :: D.NodeAddress          -- ^ Address of this node.
  , _tag            :: TVar D.NodeTag         -- ^ Tag of this node.
  , _rpcServer      :: TMVar RpcServerHandle  -- ^ RPC server of this node.
  , _graph          :: TG.LGraph              -- ^ Graph
  , _varCounter     :: TMVar Int              -- ^ Vars counter. Used to generate VarId.
  , _state          :: NodeState              -- ^ State of node.
  }

-- | Registry of nodes acting within a test network.
type NodesRegistry = TMVar (Map.Map D.NodeAddress NodeRuntime)

-- | Test runtime describing a virtual network and nodes acting within it.
-- In the testing runtime, the network environment is represented
-- as a separate thread. The network environment can relay RPC requests
-- from one node to another, it can make RPC requests to nodes,
-- and can evaluate some control requests over the test network.
data TestRuntime = TestRuntime
  { _loggerRuntime   :: LoggerRuntime     -- ^ Logger runtime.
  , _networkWorkerId :: ThreadId          -- ^ Network environment thread ID.
  , _networkControl  :: Control           -- ^ Network environment control interface
  , _registry        :: NodesRegistry     -- ^ Tag of this node.
  }
