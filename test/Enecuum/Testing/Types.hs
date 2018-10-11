{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Testing.Types where

import           Enecuum.Prelude

import qualified Data.Map                             as Map

import qualified Enecuum.Domain                       as D

-- Bad dependency of Testing from TestData.
import qualified Enecuum.TestData.TestGraph as TG

-- | Defines control requests to manipulate by nodes.
data ControlRequest
  = RpcReq D.RpcRequest         -- ^ Eval RPC request to a node.
  | RelayRpcReq                 -- ^ Relay RPC request from one node to another.
    { _from    :: D.Address     -- ^ From node
    , _to      :: D.Address     -- ^ To node
    , _request :: D.RpcRequest  -- ^ RPC request to relay
    }
  | RelayEstablishConnectionReq  -- ^ Relay request for connection to client-server.
    { _address  :: D.Address     -- ^ Server address
    }
  | EstablishConnectionReq       -- ^ Make connection of client-server type.
  | AcceptBackConnectionReq      -- ^ Accept back connection by the connection worker.
    { _connection :: BindedServer
    }
  | MessageReq D.RawData

-- | Result of evaluation of control response.
data ControlResponse
  = AsRpcResp D.RpcResponse            -- ^ RPC response wrapped into the control response.
  | AsErrorResp Text                   -- ^ Keeps an error that occured during the ControlRequest evaluation.
  | AsSuccessResp                      -- ^ Indicates success of the operation (if other responses can't be applied.)
  | AsConnectionAccepted BindedServer  -- ^ Connection accepted by the server. Server has set some binding address.

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

-- | Server handle.
data ServerHandle = ServerHandle
  { _threadId    :: ThreadId    -- ^ Server thread ID.
  , _control     :: Control     -- ^ Server control interface.
  , _nodeRuntime :: NodeRuntime -- ^ Node runtime.
  }
-- | Connection worker handle.
data ConnectionWorkerHandle = ConnectionWorkerHandle
  { _threadId    :: ThreadId    -- ^ Server thread ID.
  , _control     :: Control     -- ^ Server control interface.
  , _nodeRuntime :: NodeRuntime -- ^ Node runtime.
  , _backConnection :: TMVar D.TcpConnection -- ^ back connection to client.
  }

-- | Logger runtime. Stores messages.
data LoggerRuntime = LoggerRuntime
  { _messages     :: TVar [Text]
  }

data VarHandle = VarHandle D.VarId (TVar Any)
type NodeState = TMVar (Map.Map D.VarId VarHandle)

type BindingAddress = D.Address

-- | Specific server binded to the specific connection.
data BindedServer = BindedServer
  { _address :: BindingAddress
  , _handle :: ConnectionWorkerHandle
  }

data NodeRole = Client | Server
data NodeConnection = NodeConnection
  { _role         :: NodeRole               -- ^ Initial role of the node.
  , _bindedServer :: BindedServer           -- ^ Binded server address and handler.
  }
type NodeConnections = Map.Map BindingAddress NodeConnection

type Servers = Map.Map D.Address ServerHandle

-- | Test runtime for every node acting within a particular test runtime.
data NodeRuntime = NodeRuntime
  { _loggerRuntime   :: LoggerRuntime          -- ^ Logger runtime.
  , _networkControl  :: Control                -- ^ Control interface for virtual network.
  , _address         :: D.Address              -- ^ Address of this node.
  , _tag             :: TVar D.NodeTag         -- ^ Tag of this node.
  , _rpcServer       :: TMVar RpcServerHandle  -- ^ RPC server of this node. (TODO: make multiple RPC servers possible)
  , _connections     :: TMVar NodeConnections
  , _graph           :: TG.TestGraphVar        -- ^ Graph
  , _varCounter      :: TMVar Int              -- ^ Vars counter. Used to generate VarId.
  , _state           :: NodeState              -- ^ State of node.
  , _serversRegistry :: ServersRegistry        -- ^ Servers in the network.
  }

-- | Registry of nodes acting within a test network.
-- TODO:
-- newtype NodeID = NodeID Int
-- type NodesRegistry = TMVar (Map.Map NodeID NodeRuntime)

type NodeID = D.Address
type NodesRegistry = TMVar (Map.Map NodeID NodeRuntime)
type ServersRegistry = TMVar (Map.Map D.Address ServerHandle)

-- | Test runtime describing a virtual network and nodes acting within it.
-- In the testing runtime, the network environment is represented
-- as a separate thread. The network environment can relay RPC requests
-- from one node to another, it can make RPC requests to nodes,
-- and can evaluate some control requests over the test network.
data TestRuntime = TestRuntime
  { _loggerRuntime   :: LoggerRuntime       -- ^ Logger runtime.
  , _networkWorkerId :: ThreadId            -- ^ Network environment thread ID.
  , _networkControl  :: Control             -- ^ Network environment control interface
  , _registry        :: NodesRegistry       -- ^ Nodes registry.
  , _serversRegistry :: ServersRegistry     -- ^ Servers available in the network.
  }
