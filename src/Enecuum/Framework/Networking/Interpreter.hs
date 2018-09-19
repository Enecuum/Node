module Enecuum.Framework.Networking.Interpreter where

import Enecuum.Prelude

import           Control.Monad.Free
import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L
import qualified Enecuum.Framework.Lens             as Lens
import           Enecuum.Legacy.Service.Network.Base
import           Enecuum.Legacy.Service.Network.WebSockets.Client
import           Enecuum.Core.Logger.Interpreter
import qualified Network.WebSockets                               as WS
import           Data.Aeson as A
import qualified Data.Text       as T


-- | Interpret NetworkingL language.
--interpretNetworkingL :: L.NetworkingL a -> Eff L.NetworkModel a
interpretNetworkingL (L.OpenConnection _ _)  = do
    L.logInfo "OpenConnection cfg"
    undefined

interpretNetworkingL (L.CloseConnection _ _) = do
    L.logInfo "CloseConnection conn"
    undefined

interpretNetworkingL (L.SendRequest _ _ _)   = do
    L.logInfo "SendRequest conn req"
    undefined

interpretNetworkingL (L.EvalNetwork _ _)     = do
    L.logInfo "Eval Network"
    undefined

interpretNetworkingL (L.SendRpcRequest (ConnectInfo host port) request next) = do
    L.logInfo "Send rpc request"
    runClient host (fromEnum port) "/" $ \connect -> do
        WS.sendTextData connect $ A.encode request
        next <$> (transformEither T.pack id . A.eitherDecode) <$> WS.receiveData connect

transformEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d 
transformEither f _ (Left a)  = Left (f a)
transformEither _ f (Right a) = Right (f a)

runNetworkingL = foldFree interpretNetworkingL
