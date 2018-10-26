module Enecuum.Testing.Framework.Interpreters.Network where

import Enecuum.Prelude

import qualified Enecuum.Language                   as L
import qualified Enecuum.Testing.RLens              as RLens
import qualified Enecuum.Testing.Types              as T
import qualified Enecuum.Testing.Core.Interpreters  as Impl

-- | Interpret NetworkSendingL. Does nothing ATM.
interpretNetworkSendingL :: T.NodeRuntime -> L.NetworkSendingF a -> IO a
interpretNetworkSendingL _ (L.Multicast _ _ next) = pure $ next ()

-- | Runs NetworkSendingL language.
runNetworkSendingL :: T.NodeRuntime -> L.NetworkSendingL a -> IO a
runNetworkSendingL nodeRt = foldFree (interpretNetworkSendingL nodeRt)

-- | Interpret NetworkListeningL (with NetworkSendingL in stack). Does nothing ATM.
interpretNetworkListeningL :: T.NodeRuntime -> L.NetworkListeningF a -> IO a
interpretNetworkListeningL _ (L.WaitForSingleResponse _ _ next) = pure $ next Nothing

-- | Runs NetworkListeningL language.
runNetworkListeningL :: T.NodeRuntime -> L.NetworkListeningL a -> IO a
runNetworkListeningL nodeRt = foldFree (interpretNetworkListeningL nodeRt)

-- | Interpret Network. Runs underlying NetworkListeningL and NetworkSendingL interpreters.
interpretNetwork :: T.NodeRuntime -> L.NetworkF a -> IO a
interpretNetwork nodeRt (L.Synchronize sending listening next) = do
    runNetworkSendingL nodeRt sending
    next <$> runNetworkListeningL nodeRt listening

interpretNetwork nodeRt (L.EvalCoreEffectNetworkF coreEffect next) =
    next <$> Impl.runCoreEffect (nodeRt ^. RLens.loggerRuntime) coreEffect

-- | Runs Network language.
runNetworkL :: T.NodeRuntime -> L.NetworkL a -> IO a
runNetworkL nodeRt = foldFree (interpretNetwork nodeRt)
