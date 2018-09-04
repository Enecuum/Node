module Enecuum.Framework.Testing.Runtime.Types where

import Enecuum.Prelude

import qualified Enecuum.Domain                as D

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
  {

  }

mkEmptyNodeRuntime :: NodeAddress -> IO NodeRuntime
mkEmptyNodeRuntime addr = do
  tag   <- newTVarIO ("" :: Text)
  handle <- newEmptyTMVarIO
  pure $ NodeRuntime addr tag handle

mkTestRuntime :: IO TestRuntime
mkTestRuntime = pure TestRuntime
