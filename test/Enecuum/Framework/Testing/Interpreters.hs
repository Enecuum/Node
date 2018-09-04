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
  :: L.NetworkSendingL a
  -> Eff '[SIO, Exc SomeException] a
interpretNetworkSendingL (L.Multicast cfg req) = safeIO $ print "L.Multicast cfg req"

interpretNetworkListeningL
  :: L.NetworkListeningL a
  -> Eff '[L.NetworkSendingL, SIO, Exc SomeException] a
interpretNetworkListeningL (L.WaitForSingleResponse cfg timeout) = do
  safeIO $ print "L.WaitForSingleResponse cfg timeout"
  pure Nothing

interpretNetworkListeningL'
  :: L.NetworkListeningL a
  -> Eff '[SIO, Exc SomeException] a
interpretNetworkListeningL' (L.WaitForSingleResponse cfg timeout) = do
  safeIO $ print "L.WaitForSingleResponse cfg timeout"
  pure Nothing

interpretNetworkSyncL
  :: L.NetworkSyncL a
  -> Eff '[L.NetworkListeningL, L.NetworkSendingL, SIO, Exc SomeException] a
interpretNetworkSyncL (L.Synchronize sending listening) = do
  safeIO $ print "Synchronize"
  raise $ raise $ handleRelay pure ( (>>=) . interpretNetworkSendingL )    sending
  raise $ raise $ handleRelay pure ( (>>=) . interpretNetworkListeningL' ) listening

interpretNetworkingL
  :: L.NetworkingL a
  -> Eff '[L.NetworkSyncL, L.NetworkListeningL, L.NetworkSendingL, SIO, Exc SomeException] a
interpretNetworkingL (L.OpenConnection cfg) = do
  safeIO $ print "OpenConnection cfg"
  pure $ Just D.Connection
interpretNetworkingL (L.CloseConnection conn) = do
  safeIO $ print "CloseConnection conn"
  pure ()
interpretNetworkingL (L.SendRequest conn req) = do
  safeIO $ print "SendRequest conn req"
  pure D.RpcResponse
interpretNetworkingL (L.EvalNetwork networkAction) = do
  safeIO $ print "Eval Network"
  networkAction

interpretNodeL
  :: L.NodeL a
  -> Eff '[L.NetworkingL, L.NetworkSyncL, L.NetworkListeningL, L.NetworkSendingL, SIO, Exc SomeException] a
interpretNodeL (L.Dummy) = safeIO $ print "L.Dummy"

runNodeL
  :: Eff L.NodeModel a
  -> Eff '[SIO, Exc SomeException] a
runNodeL
  = handleRelay pure ( (>>=) . interpretNetworkSendingL )
  . handleRelay pure ( (>>=) . interpretNetworkListeningL )
  . handleRelay pure ( (>>=) . interpretNetworkSyncL )
  . handleRelay pure ( (>>=) . interpretNetworkingL )
  . handleRelay pure ( (>>=) . interpretNodeL )


interpretNodeDefinitionL
  :: L.NodeDefinitionL a
  -> Eff '[SIO, Exc SomeException] a
interpretNodeDefinitionL (L.NodeTag nodeTag')          = safeIO $ print "Node tag" --nodeTag .= nodeTag'
interpretNodeDefinitionL (L.Initialization initScript) = do
  safeIO $ print "Initialization"
  runNodeL initScript
interpretNodeDefinitionL (L.Serving handlersF)         = do
  safeIO $ print "Serving handlersF"
  pure $ D.ServerDef

runNodeDefinitionL
  :: Eff '[L.NodeDefinitionL, SIO, Exc SomeException] a
  -> Eff '[SIO, Exc SomeException] a
runNodeDefinitionL = handleRelay pure ( (>>=) . interpretNodeDefinitionL )

runNode
  :: Eff '[L.NodeDefinitionL, SIO, Exc SomeException] a
  -> IO a
runNode x = runSafeIO $ runNodeDefinitionL x
