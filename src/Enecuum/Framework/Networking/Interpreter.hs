module Enecuum.Framework.Networking.Interpreter where

import Enecuum.Prelude

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L
import qualified Enecuum.Framework.Lens             as Lens
import           Enecuum.Legacy.Service.Network.Base
import           Enecuum.Legacy.Service.Network.WebSockets.Client
import qualified Network.WebSockets                               as WS
import           Data.Aeson as A


-- | Interpret NetworkingL language.
interpretNetworkingL :: L.NetworkingL a -> Eff L.NetworkModel a
interpretNetworkingL (L.OpenConnection _)  = do
    L.logInfo "OpenConnection cfg"
    undefined

interpretNetworkingL (L.CloseConnection _) = do
    L.logInfo "CloseConnection conn"
    undefined

interpretNetworkingL (L.SendRequest _ _)   = do
    L.logInfo "SendRequest conn req"
    undefined

interpretNetworkingL (L.EvalNetwork _)     = do
    L.logInfo "Eval Network"
    undefined

interpretNetworkingL (L.SendRpcRequest (ConnectInfo host port) request) = do
    L.logInfo "Send rpc request"
    safeIO $ runClient host (fromEnum port) "/" $ \connect -> do
        WS.sendTextData connect $ A.encode request
        A.decode <$> WS.receiveData connect
