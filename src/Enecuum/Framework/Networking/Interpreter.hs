module Enecuum.Framework.Networking.Interpreter where

import Enecuum.Prelude

import           Control.Monad.Free
import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L
import qualified Enecuum.Framework.Lens             as Lens
import           Enecuum.Legacy.Service.Network.Base
import           Enecuum.Legacy.Service.Network.WebSockets.Client
import qualified Network.WebSockets                               as WS
import           Data.Aeson as A
import qualified Data.Text       as T

-- | Interpret NetworkingL language.
interpretNetworkingL :: L.NetworkingF a -> IO a
interpretNetworkingL (L.OpenConnection _ _)  = do
    error "interpretNetworkingL OpenConnection not implemented."

interpretNetworkingL (L.CloseConnection _ _) = do
    error "interpretNetworkingL CloseConnection not implemented."

interpretNetworkingL (L.SendRequest _ _ _)   = do
    error "interpretNetworkingL SendRequest not implemented."

interpretNetworkingL (L.EvalNetwork _ _)     = do
    error "interpretNetworkingL EvalNetwork not implemented."

interpretNetworkingL (L.SendRpcRequest (ConnectInfo host port) request next) = do
    runClient host (fromEnum port) "/" $ \connect -> do
        WS.sendTextData connect $ A.encode request
        next <$> (transformEither T.pack id . A.eitherDecode) <$> WS.receiveData connect

transformEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
transformEither f _ (Left a)  = Left (f a)
transformEither _ f (Right a) = Right (f a)

-- | Run Networking language.
runNetworkingL :: L.NetworkingL a -> IO a
runNetworkingL = foldFree interpretNetworkingL
