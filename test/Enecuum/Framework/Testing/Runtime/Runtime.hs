module Enecuum.Framework.Testing.Runtime.Runtime where

import           Enecuum.Prelude

import qualified Data.Map as Map

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L
import qualified Enecuum.Framework.Lens             as Lens

import           Enecuum.Core.Testing.Runtime.Types
import           Enecuum.Framework.Testing.Runtime.Types
import qualified Enecuum.Framework.Testing.Runtime.Lens as RLens

createEmptyNodeRuntime :: NodeAddress -> IO NodeRuntime
createEmptyNodeRuntime addr = do
  tag   <- newTVarIO ("" :: Text)
  handle <- newEmptyTMVarIO
  pure $ NodeRuntime addr tag handle

createTestRuntime :: IO TestRuntime
createTestRuntime = do
  loggerRt <- createLoggerRuntime
  nodes <- newTMVarIO Map.empty
  pure $ TestRuntime loggerRt nodes

registerNode
  :: TestRuntime
  -> NodeAddress
  -> NodeRuntime
  -> IO ()
registerNode testRt addr nodeRt = do
  nodes <- atomically $ takeTMVar $ testRt ^. RLens.nodes
  case Map.lookup addr nodes of
    Just _ -> error "Node is already registered."
    Nothing -> atomically
      $ putTMVar (testRt ^. RLens.nodes)
      $ Map.insert addr nodeRt nodes

findNode
  :: TestRuntime
  -> NodeAddress
  -> IO (Maybe NodeRuntime)
findNode testRt addr = do
  nodes <- atomically $ readTMVar $ testRt ^. RLens.nodes
  pure $ Map.lookup addr nodes
