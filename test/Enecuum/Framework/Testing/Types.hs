{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Framework.Testing.Types where

import Enecuum.Prelude

import qualified Data.Map as Map

import qualified Enecuum.Domain                as D
import           Enecuum.Core.Testing.Runtime.Types

data RpcServerControlRequest = RpcServerControlRpcRequest D.RpcRequest
data RpcServerControlResponse
  = RpcServerControlRpcResponse D.RpcResponse
  | RpcServerControlErrorResponse Text

data NetworkControlRequest = NetworkControlRelayRpcRequest
data NetworkControlResponse = NetworkControlRpcResponse

data NetworkControl = NetworkControl
  { _request  :: TMVar NetworkControlRequest
  , _response :: TMVar NetworkControlResponse
  }

data RpcServerControl = RpcServerControl
  { _request  :: TMVar RpcServerControlRequest
  , _response :: TMVar RpcServerControlResponse
  }

data RpcServerHandle = RpcServerHandle
  { _threadId :: ThreadId
  , _control  :: RpcServerControl
  }

data NodeRuntime = NodeRuntime
  { _loggerRuntime  :: LoggerRuntime
  , _networkControl :: NetworkControl
  , _address        :: D.NodeAddress
  , _tag            :: TVar D.NodeTag
  , _rpcServer      :: TMVar RpcServerHandle
  }

data TestRuntime = TestRuntime
  { _loggerRuntime   :: LoggerRuntime
  , _networkWorkerId :: ThreadId
  , _networkControl  :: NetworkControl
  , _nodes           :: TMVar (Map.Map D.NodeAddress NodeRuntime)
  }
