module Enecuum.Framework.Testing.Interpreters where

import           Data.Text                                ( Text )
import           Eff                                      ( Eff )
import qualified Data.Aeson                    as A
import           Data.Aeson                               ( ToJSON
                                                          , FromJSON
                                                          )
import qualified Data.ByteString.Lazy          as BS
import           GHC.Generics                             ( Generic )
import           Control.Newtype.Generics                 ( Newtype
                                                          , O
                                                          , pack
                                                          , unpack
                                                          )
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

import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import           Enecuum.Framework.Testing.Runtime

interpretNetworkSendingL
  :: NodeRuntime
  -> L.NetworkSendingL a
  -> Eff '[SIO, Exc SomeException] a
interpretNetworkSendingL rt (L.Multicast cfg req) = safeIO $ print "L.Multicast cfg req"

interpretNetworkListeningL
  :: NodeRuntime
  -> L.NetworkListeningL a
  -> Eff '[L.NetworkSendingL, SIO, Exc SomeException] a
interpretNetworkListeningL rt (L.WaitForSingleResponse cfg timeout) = do
  safeIO $ print "L.WaitForSingleResponse cfg timeout"
  pure Nothing

interpretNetworkListeningL'
  :: NodeRuntime
  -> L.NetworkListeningL a
  -> Eff '[SIO, Exc SomeException] a
interpretNetworkListeningL' rt (L.WaitForSingleResponse cfg timeout) = do
  safeIO $ print "L.WaitForSingleResponse cfg timeout"
  pure Nothing

interpretNetworkSyncL
  :: NodeRuntime
  -> L.NetworkSyncL a
  -> Eff '[L.NetworkListeningL, L.NetworkSendingL, SIO, Exc SomeException] a
interpretNetworkSyncL rt (L.Synchronize sending listening) = do
  safeIO $ print "Synchronize"
  raise $ raise $ handleRelay pure ( (>>=) . interpretNetworkSendingL rt )    sending
  raise $ raise $ handleRelay pure ( (>>=) . interpretNetworkListeningL' rt ) listening

interpretNetworkingL
  :: NodeRuntime
  -> L.NetworkingL a
  -> Eff '[L.NetworkSyncL, L.NetworkListeningL, L.NetworkSendingL, SIO, Exc SomeException] a
interpretNetworkingL rt (L.OpenConnection cfg) = do
  safeIO $ print "OpenConnection cfg"
  pure $ Just D.Connection
interpretNetworkingL rt (L.CloseConnection conn) = do
  safeIO $ print "CloseConnection conn"
  pure ()
interpretNetworkingL rt (L.SendRequest conn req) = do
  safeIO $ print "SendRequest conn req"
  pure D.RpcResponse
interpretNetworkingL rt (L.EvalNetwork networkAction) = do
  safeIO $ print "Eval Network"
  networkAction

interpretNodeL
  :: NodeRuntime
  -> L.NodeL a
  -> Eff '[L.NetworkingL, L.NetworkSyncL, L.NetworkListeningL, L.NetworkSendingL, SIO, Exc SomeException] a
interpretNodeL rt (L.Dummy) = safeIO $ print "L.Dummy"

runNodeL
  :: NodeRuntime
  -> Eff L.NodeModel a
  -> Eff '[SIO, Exc SomeException] a
runNodeL rt
  = handleRelay pure ( (>>=) . interpretNetworkSendingL rt )
  . handleRelay pure ( (>>=) . interpretNetworkListeningL rt )
  . handleRelay pure ( (>>=) . interpretNetworkSyncL rt )
  . handleRelay pure ( (>>=) . interpretNetworkingL rt )
  . handleRelay pure ( (>>=) . interpretNodeL rt )


interpretNodeDefinitionL
  :: NodeRuntime
  -> L.NodeDefinitionL a
  -> Eff '[SIO, Exc SomeException] a
interpretNodeDefinitionL rt (L.NodeTag nodeTag')          = safeIO $ print "Node tag" --nodeTag .= nodeTag'
interpretNodeDefinitionL rt (L.Initialization initScript) = do
  safeIO $ print "Initialization"
  runNodeL rt initScript
interpretNodeDefinitionL rt (L.Serving handlersF)         = do
  safeIO $ print "Serving handlersF"
  pure $ D.ServerDef

runNodeDefinitionL
  :: NodeRuntime
  -> Eff '[L.NodeDefinitionL, SIO, Exc SomeException] a
  -> Eff '[SIO, Exc SomeException] a
runNodeDefinitionL rt = handleRelay pure ( (>>=) . interpretNodeDefinitionL rt )

runNode
  :: TestRuntime
  -> Eff '[L.NodeDefinitionL, SIO, Exc SomeException] a
  -> IO a
runNode rt nodeAddr = do
  
  runSafeIO $ runNodeDefinitionL rt x
