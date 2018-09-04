module Enecuum.Framework.Testing.Runtime.NodeDefinition.Impl where

import Enecuum.Prelude

import qualified Data.Aeson                    as A
import qualified Data.ByteString.Lazy          as BS
import           Control.Monad.IO.Class                                         (MonadIO, liftIO)
import           Control.Monad.Trans.Class                                      (lift)
import           Control.Monad.State.Class                                      (MonadState, get, put)
import           Control.Exception                                              (SomeException)
import           Eff                                                            (Eff, Member, handleRelay, runM, send, raise, replaceRelay)
import           Eff.Exc                                                        (Exc)
import qualified Eff.State                                                      as S
import           Eff.State                                                      (State, get, put)
import           Eff.State.Pure                                                 (evalState)
import           Eff.Reader                                                     (ask)
import           Eff.Reader.Pure                                                (Reader, runReader)
import           Eff.SafeIO                                                     (SIO, runSafeIO, safeIO)
import           Eff.Extra ()

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L

import           Enecuum.Framework.Testing.Runtime.Types
import           Enecuum.Framework.Testing.Runtime.STM
import           Enecuum.Framework.Testing.Runtime.NetworkModel.Impl
import           Enecuum.Framework.Testing.Runtime.NodeModel.Impl
import           Enecuum.Framework.Testing.Runtime.Server (testRpcServer)
import qualified Enecuum.Framework.Testing.Runtime.Lens     as RLens


interpretNodeDefinitionL
  :: NodeRuntime
  -> L.NodeDefinitionL a
  -> Eff '[SIO, Exc SomeException] a
interpretNodeDefinitionL rt (L.NodeTag tag) = do
  safeIO $ print "Node tag: "
  safeIO $ print tag
  safeIO $ atomically $ setNodeTag rt tag
interpretNodeDefinitionL rt (L.Initialization initScript) = do
  safeIO $ print "Initialization"
  runNodeL rt initScript
interpretNodeDefinitionL rt (L.Serving handlersF) = do
  safeIO $ print "Serving handlersF"

  control <- safeIO newEmptyMVar
  serverThreadId <- safeIO $ forkIO $ testRpcServer control handlersF

  pure $ D.ServerDef

runNodeDefinitionL
  :: NodeRuntime
  -> Eff '[L.NodeDefinitionL, SIO, Exc SomeException] a
  -> Eff '[SIO, Exc SomeException] a
runNodeDefinitionL rt = handleRelay pure ( (>>=) . interpretNodeDefinitionL rt )

runNode
  :: TestRuntime
  -> NodeAddress
  -> Eff '[L.NodeDefinitionL, SIO, Exc SomeException] a
  -> IO a
runNode rt nodeAddr scenario = do
  nodeRt <- mkEmptyNodeRuntime nodeAddr
  runSafeIO $ runNodeDefinitionL nodeRt scenario
