module Enecuum.Framework.Testing.Node.Interpreters.Network where

import Enecuum.Prelude

import qualified Data.Aeson                         as A
import qualified Data.ByteString.Lazy               as BS
import           Control.Monad.Free                 (foldFree)

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L

import qualified Enecuum.Framework.Lens             as Lens
import qualified Enecuum.Framework.Testing.Lens     as RLens
import           Enecuum.Framework.Testing.Types
import qualified Enecuum.Core.Testing.Runtime.Interpreters as Impl

-- | Interpret NetworkSendingL. Does nothing ATM.
interpretNetworkSendingL
  :: NodeRuntime
  -> L.NetworkSendingF a
  -> IO a
interpretNetworkSendingL nodeRt (L.Multicast cfg req next) = do
  Impl.runLoggerL (nodeRt ^. RLens.loggerRuntime) $ L.logInfo "L.Multicast cfg req"
  pure $ next ()

-- | Runs NetworkSendingL language.
runNetworkSendingL :: NodeRuntime -> L.NetworkSendingL a -> IO a
runNetworkSendingL nodeRt = foldFree (interpretNetworkSendingL nodeRt)

-- | Interpret NetworkListeningL (with NetworkSendingL in stack). Does nothing ATM.
interpretNetworkListeningL
  :: NodeRuntime
  -> L.NetworkListeningF a
  -> IO a
interpretNetworkListeningL nodeRt (L.WaitForSingleResponse cfg timeout next) = do
  Impl.runLoggerL (nodeRt ^. RLens.loggerRuntime) $ L.logInfo "L.WaitForSingleResponse cfg timeout"
  pure $ next Nothing

-- | Runs NetworkListeningL language.
runNetworkListeningL :: NodeRuntime -> L.NetworkListeningL a -> IO a
runNetworkListeningL nodeRt = foldFree (interpretNetworkListeningL nodeRt)

-- | Interpret Network. Runs underlying NetworkListeningL and NetworkSendingL interpreters.
interpretNetwork
  :: NodeRuntime
  -> L.NetworkF a
  -> IO a
interpretNetwork nodeRt (L.Synchronize sending listening next) = do
  Impl.runLoggerL (nodeRt ^. RLens.loggerRuntime) $ L.logInfo "Synchronize"
  runNetworkSendingL nodeRt sending
  next <$> runNetworkListeningL nodeRt listening

interpretNetwork nodeRt (L.EvalCoreEffectNetworkF coreEffect next) =
  next <$> Impl.runCoreEffect (nodeRt ^. RLens.loggerRuntime) coreEffect

-- | Runs Network language.
runNetworkL :: NodeRuntime -> L.NetworkL a -> IO a
runNetworkL nodeRt = foldFree (interpretNetwork nodeRt)
