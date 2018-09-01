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
import           Eff                                                            (Eff, Member, handleRelay, runM)
import           Eff.Exc                                                        (Exc)
import           Eff.Reader                                                     (ask)
import           Eff.Reader.Pure                                                (Reader, runReader)
import           Eff.SafeIO                                                     (SIO, runSafeIO)


import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
-- import qualified Enecuum.Framework.Runtime     as R


-- data NetworkSendingL a where
--   -- Unicast   :: D.NetworkConfig -> D.NetworkRequest -> NetworkSendingL ()
--   -- Broadcast :: D.NetworkConfig -> D.NetworkRequest -> NetworkSendingL ()
--   Multicast :: D.NetworkConfig -> D.NetworkRequest -> NetworkSendingL ()

-- data NetworkListeningL a where
--   WaitForSingleResponse :: D.NetworkConfig -> D.WaitingTimeout -> NetworkListeningL (Maybe D.NetworkResponse)
--   WaitForResponses      :: D.NetworkConfig -> D.WaitingTimeout -> NetworkListeningL [D.NetworkResponse]

-- data NetworkSyncL a where
--   Synchronize :: Eff '[NetworkSendingL] () -> Eff '[NetworkListeningL] a -> NetworkSyncL a




interpretNetworkSendingL
  :: L.NetworkSendingL a
  -> Eff '[IO] a
interpretNetworkSendingL (L.Multicast cfg req) = error "L.Multicast cfg req"

runNetworkSendingL
  :: Eff '[L.NetworkSendingL, IO] a
  -> IO a
runNetworkSendingL = runM . handleRelay pure ( (>>=) . interpretNetworkSendingL )

interpretNetworkListeningL
  :: L.NetworkListeningL a
  -> Eff '[IO] a
interpretNetworkListeningL (L.WaitForSingleResponse cfg timeout) = error "L.WaitForSingleResponse cfg timeout"

runNetworkListeningL
  :: Eff '[L.NetworkListeningL, IO] a
  -> IO a
runNetworkListeningL = runM . handleRelay pure ( (>>=) . interpretNetworkListeningL )




interpretNodeDefinitionL
  :: L.NodeDefinitionL a
  -> Eff '[IO] a
interpretNodeDefinitionL (L.NodeTag nodeTag)           = error "interpretNodeDefinition (NodeTag nodeTag)"
interpretNodeDefinitionL (L.Initialization initScript) = error "Initialization initScript"
interpretNodeDefinitionL (L.Serving handlersF)         = error "Serving handlersF"

runNodeDefinitionModel
  :: Eff L.NodeDefinitionModel a
  -> IO a
runNodeDefinitionModel = runM . handleRelay pure ( (>>=) . interpretNodeDefinitionL )





runNode = runNodeDefinitionModel