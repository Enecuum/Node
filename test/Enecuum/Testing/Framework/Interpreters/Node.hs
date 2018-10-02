module Enecuum.Testing.Framework.Interpreters.Node where

import Enecuum.Prelude

import qualified Enecuum.Domain                                    as D
import qualified Enecuum.Language                                  as L

import qualified Enecuum.Testing.RLens                             as RLens
import qualified Enecuum.Testing.Types                             as T
import qualified Enecuum.Testing.Core.Interpreters                 as Impl
import qualified Enecuum.Testing.Framework.Interpreters.Networking as Impl
import qualified Enecuum.Testing.Framework.Interpreters.State      as Impl

-- | Relay request from this node to the network environment.
establishConnection
  :: T.NodeRuntime
  -> D.Address
  -> IO ()
establishConnection nodeRt to = do
  atomically
      $ putTMVar (nodeRt ^. RLens.networkControl . RLens.request)
      $ T.EstablishConnectionReq (nodeRt ^. RLens.address) to
  controlResponse <- atomically
      $ takeTMVar (nodeRt ^. RLens.networkControl . RLens.response)
  case controlResponse of
    T.AsConnectionEstablished -> pure ()
    T.AsErrorResp msg         -> error $ "Network control error: " <> msg
    _                         -> error "Invalid network control result."


-- | Interpret NodeL.
interpretNodeL :: T.NodeRuntime -> L.NodeF a -> IO a

interpretNodeL nodeRt (L.EvalStateAtomically statefulAction next) = do
  next <$> (atomically $ Impl.runStateL nodeRt statefulAction)

interpretNodeL nodeRt (L.EvalGraphIO (L.GraphAction _ ioRunner act) next) = do
  next <$> ioRunner act

interpretNodeL nodeRt (L.EvalNetworking networkingAction next) =
  next <$> Impl.runNetworkingL nodeRt networkingAction

interpretNodeL nodeRt (L.EvalCoreEffectNodeF coreEffect next) =
  next <$> Impl.runCoreEffect (nodeRt ^. RLens.loggerRuntime) coreEffect

interpretNodeL nodeRt (L.OpenConnection addr handlers next) = do
  establishConnection nodeRt addr
  cmd <- newEmptyTMVarIO
  pure $ next $ D.NetworkConnection cmd

interpretNodeL nodeRt (L.CloseConnection _ next) =
  error "CloseConnection not implemented."

-- interpretNetworkingL nodeRt (L.Send (D.NetworkConnection conn) req next) = do
--   -- next <$> D.sendRequest conn req
--   error "Send not implemented."

-- | Runs node language.
runNodeL :: T.NodeRuntime -> L.NodeL a -> IO a
runNodeL nodeRt = foldFree (interpretNodeL nodeRt)
