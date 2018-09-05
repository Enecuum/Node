module Enecuum.Framework.Testing.Runtime.Runtime where

import           Enecuum.Prelude

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L
import qualified Enecuum.Framework.Lens             as Lens

import           Enecuum.Framework.Testing.Runtime.Types

createEmptyNodeRuntime :: NodeAddress -> IO NodeRuntime
createEmptyNodeRuntime addr = do
  tag   <- newTVarIO ("" :: Text)
  handle <- newEmptyTMVarIO
  pure $ NodeRuntime addr tag handle

createTestRuntime :: IO TestRuntime
createTestRuntime = do
  loggerRt <- createLoggerRuntime
  pure $ TestRuntime loggerRt

registerNode
  :: TestRuntime
  -> NodeAddress
  -> NodeRuntime
  -> IO ()
registerNode rt addr = do


findNode
  :: TestRuntime
  -> NodeAddress
  -> IO (Maybe NodeRuntime)
findNode rt addr = pure Nothing
