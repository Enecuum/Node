{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Framework.Testing.Types where

import Enecuum.Prelude

import qualified Data.Map as Map

import qualified Enecuum.Domain                as D
import           Enecuum.Core.Testing.Runtime.Types

data ControlRequest
  = RpcRequest D.RpcRequest
  | RelayRpcRequest
    { _from    :: D.NodeAddress
    , _to      :: D.NodeAddress
    , _request :: D.RpcRequest
    }

data ControlResponse
  = AsRpcResponse D.RpcResponse
  | AsErrorResponse Text

data Control = Control
  { _request  :: TMVar ControlRequest
  , _response :: TMVar ControlResponse
  }

data RpcServerHandle = RpcServerHandle
  { _threadId :: ThreadId
  , _control  :: Control
  }

data NodeRuntime = NodeRuntime
  { _loggerRuntime  :: LoggerRuntime
  , _networkControl :: Control
  , _address        :: D.NodeAddress
  , _tag            :: TVar D.NodeTag
  , _rpcServer      :: TMVar RpcServerHandle
  }

type NodesRegistry = TMVar (Map.Map D.NodeAddress NodeRuntime)

data TestRuntime = TestRuntime
  { _loggerRuntime   :: LoggerRuntime
  , _networkWorkerId :: ThreadId
  , _networkControl  :: Control
  , _registry        :: NodesRegistry
  }
