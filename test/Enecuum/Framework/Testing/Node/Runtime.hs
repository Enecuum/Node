-- | This module contains functions to work with node runtime.
module Enecuum.Framework.Testing.Node.Runtime where

import           Enecuum.Prelude

import           Eff.SafeIO                         ( runSafeIO )
import qualified Data.Map as Map

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L
import qualified Enecuum.Framework.Lens             as Lens

import           Enecuum.Core.Testing.Runtime.Types

import           Enecuum.Framework.Testing.Types
import qualified Enecuum.Framework.Testing.Lens as RLens

import           Enecuum.Core.Testing.Runtime.Logger.Impl

import           Enecuum.Framework.Testing.Environment.TestRuntime
import           Enecuum.Framework.Testing.Node.Interpreters.NodeDefinition

-- | Creates node runtime.
createEmptyNodeRuntime
  :: LoggerRuntime
  -> Control
  -> D.NodeAddress
  -> IO NodeRuntime
createEmptyNodeRuntime loggerRt networkControl addr = do
  tag   <- newTVarIO ("" :: Text)
  handle <- newEmptyTMVarIO
  pure $ NodeRuntime loggerRt networkControl addr tag handle

-- | Starts node using NodeDefinitionL.
startNode
  :: TestRuntime
  -> D.NodeAddress
  -> Eff '[L.NodeDefinitionL, L.LoggerL, SIO, Exc SomeException] ()
  -> IO NodeRuntime
startNode testRt nodeAddr scenario = do
  nodeRt <- createEmptyNodeRuntime (testRt ^. RLens.loggerRuntime) (testRt ^. RLens.networkControl) nodeAddr
  runSafeIO
    $ runLoggerL (testRt ^. RLens.loggerRuntime)
    $ runNodeDefinitionL nodeRt scenario
  registerNode (testRt ^. RLens.registry) nodeAddr nodeRt
  pure nodeRt
