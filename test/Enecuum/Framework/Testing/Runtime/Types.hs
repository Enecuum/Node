module Enecuum.Framework.Testing.Runtime.Types where

import Enecuum.Prelude

import qualified Data.Map as Map

import qualified Enecuum.Domain                as D
import           Enecuum.Core.Testing.Runtime.Types

type NodeAddress = Text

data ControlRequest = RpcRequest D.RawData
data ControlResponse = Ack

data NodeRpcServerControl = NodeRpcServerControl
  { _request  :: TMVar ControlRequest
  , _response :: TMVar ControlResponse
  }

data NodeRpcServerHandle = NodeRpcServerHandle
  { _threadId :: ThreadId
  , _control  :: NodeRpcServerControl
  }

data NodeRuntime = NodeRuntime
  { _address   :: NodeAddress
  , _tag       :: TVar D.NodeTag
  , _rpcServer :: TMVar NodeRpcServerHandle
  }

data TestRuntime = TestRuntime
  { _loggerRuntime :: LoggerRuntime
  , _nodes         :: Map.Map NodeAddress NodeRuntime
  }
