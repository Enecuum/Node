module Enecuum.Framework.Testing.Interpreters where

import           Data.Text                                ( Text )
import           Eff                                      ( Eff )
import qualified Data.Aeson                    as A
import           Data.Aeson                               ( ToJSON
                                                          , FromJSON
                                                          )
import           Lens.Micro                       ( (^.), (.~) )
import           Lens.Micro.Mtl                       ((.=) )
import           Lens.Micro.TH                       ( makeLenses )
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
import           Eff                                                            (Eff, Member, handleRelay, runM, send)
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
  -> Eff '[State RuntimeSt, SIO, Exc SomeException] a
interpretNetworkSendingL (L.Multicast cfg req) = error "L.Multicast cfg req"

runNetworkSendingL
  :: Eff '[L.NetworkSendingL, State RuntimeSt, SIO, Exc SomeException] a
  -> Eff '[State RuntimeSt, SIO, Exc SomeException] a
runNetworkSendingL = handleRelay pure ( (>>=) . interpretNetworkSendingL )


-- interpretNetworkListeningL
--   :: L.NetworkListeningL a
--   -> Eff '[State RuntimeSt, SIO] a
-- interpretNetworkListeningL (L.WaitForSingleResponse cfg timeout) = error "L.WaitForSingleResponse cfg timeout"

-- runNetworkListeningL
--   :: Eff '[L.NetworkListeningL, State RuntimeSt, SIO] a
--   -> IO a
-- runNetworkListeningL = runM . handleRelay pure ( (>>=) . interpretNetworkListeningL )


-- interpretNetworkSyncL
--   :: L.NetworkSyncL a
--   -> Eff '[State RuntimeSt, SIO] a
-- interpretNetworkSyncL (L.Synchronize networkSending networkListening) = error "L.Synchronize networkSending networkListening"

-- runNetworkSyncL
--   :: Eff '[L.NetworkSyncL, State RuntimeSt, SIO] a
--   -> IO a
-- runNetworkSyncL = runM . handleRelay pure ( (>>=) . interpretNetworkSyncL )

-- interpretNodeL
--   :: L.NodeL a
--   -> Eff '[State RuntimeSt, SIO] a
-- interpretNodeL (L.Dummy) = error "L.Dummy"

-- runNodeL
--   :: Eff '[L.NodeL, State RuntimeSt, SIO] a
--   -> IO a
-- runNodeL = runM . handleRelay pure ( (>>=) . interpretNodeL )



interpretNodeDefinitionL
  :: L.NodeDefinitionL a
  -> Eff '[State RuntimeSt, SIO, Exc SomeException] a
interpretNodeDefinitionL (L.NodeTag nodeTag')          = nodeTag .= nodeTag'
interpretNodeDefinitionL (L.Initialization initScript) = error "Initialization initScript"
interpretNodeDefinitionL (L.Serving handlersF)         = error "Serving handlersF"

runNodeDefinitionModel
  :: Eff (L.NodeDefinitionModel (State RuntimeSt)) a
  -> Eff '[State RuntimeSt, SIO, Exc SomeException] a
runNodeDefinitionModel = handleRelay pure ( (>>=) . interpretNodeDefinitionL )





runNode x = runSafeIO $ evalState defaultRuntimeSt (runNodeDefinitionModel x)