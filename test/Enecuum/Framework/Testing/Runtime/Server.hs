module Enecuum.Framework.Testing.Runtime.Server where

import           Enecuum.Prelude

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L
import qualified Enecuum.Framework.Lens             as Lens

import           Enecuum.Framework.Testing.Runtime.Types
import           Enecuum.Framework.Testing.Runtime.STM
import           Enecuum.Framework.Testing.Runtime.NodeModel.Impl
import qualified Enecuum.Framework.Testing.Runtime.Lens as RLens


testRpcServer
  :: MVar NodeRpcServerControl
  -> L.HandlersF
  -> IO ()
testRpcServer _ _ = pure ()
