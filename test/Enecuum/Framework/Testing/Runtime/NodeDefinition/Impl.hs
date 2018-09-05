module Enecuum.Framework.Testing.Runtime.NodeDefinition.Impl where

import Enecuum.Prelude

import           Eff                                ( Eff, Member, handleRelay, runM, send, raise, replaceRelay )
import           Eff.SafeIO                         ( runSafeIO )

import           Data.Text                          ( append )
import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L

import           Enecuum.Core.Testing.Runtime.Types
import           Enecuum.Core.Testing.Runtime.Logger.Impl
import           Enecuum.Framework.Testing.Runtime.Types
import           Enecuum.Framework.Testing.Runtime.STM
import           Enecuum.Framework.Testing.Runtime.NetworkModel.Impl
import           Enecuum.Framework.Testing.Runtime.NodeModel.Impl
import           Enecuum.Framework.Testing.Runtime.Server             (startNodeRpcServer)
import qualified Enecuum.Framework.Testing.Runtime.Lens               as RLens


interpretNodeDefinitionL
  :: NodeRuntime
  -> L.NodeDefinitionL a
  -> Eff '[L.LoggerL, SIO, Exc SomeException] a
interpretNodeDefinitionL rt (L.NodeTag tag) = do
  L.logInfo $ append "Node tag: " tag
  safeIO $ atomically $ setNodeTag rt tag
interpretNodeDefinitionL rt (L.Initialization initScript) = do
  L.logInfo "Initialization"
  runNodeModel rt initScript
interpretNodeDefinitionL rt (L.Serving handlersF) = do
  L.logInfo "Serving handlersF"
  safeIO $ startNodeRpcServer rt handlersF


runNodeDefinitionL
  :: NodeRuntime
  -> Eff '[L.NodeDefinitionL, L.LoggerL, SIO, Exc SomeException] a
  -> Eff '[L.LoggerL, SIO, Exc SomeException] a
runNodeDefinitionL rt = handleRelay pure ( (>>=) . interpretNodeDefinitionL rt )

createNode
  :: TestRuntime
  -> NodeAddress
  -> Eff '[L.NodeDefinitionL, L.LoggerL, SIO, Exc SomeException] ()
  -> IO NodeRuntime
createNode rt nodeAddr scenario = do
  nodeRt <- createEmptyNodeRuntime nodeAddr
  runSafeIO
    $ runLoggerL (rt ^. RLens.loggerRuntime)
    $ runNodeDefinitionL nodeRt scenario
  registerNode rt nodeAddr
  pure nodeRt
