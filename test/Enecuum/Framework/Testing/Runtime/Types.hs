module Enecuum.Framework.Testing.Runtime.Types where

import Enecuum.Prelude

import qualified Enecuum.Domain                as D

type NodeAddress = Text

data NodeRpcServerControl = NodeRpcServerControl
  {

  }

data NodeServerHandle = NodeServerHandle
  { _serverThreadId :: ThreadId

  }

data NodeRuntime = NodeRuntime
  { _nodeAddress :: NodeAddress
  , _nodeTag :: TVar D.NodeTag
  , _serverHanlde :: TVar (Maybe NodeServerHandle)
  }

data TestRuntime = TestRuntime
  {

  }

mkEmptyNodeRuntime :: NodeAddress -> IO NodeRuntime
mkEmptyNodeRuntime addr = do
  tag <- newTVarIO ("" :: Text)
  serverHandle <- newTVarIO Nothing
  pure $ NodeRuntime addr tag serverHandle

mkTestRuntime :: IO TestRuntime
mkTestRuntime = pure TestRuntime
