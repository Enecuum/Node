module Enecuum.Framework.Networking.Interpreter where

import Enecuum.Prelude

import           Data.Aeson as A
import qualified Data.Text as T

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L
import           Enecuum.Legacy.Service.Network.WebSockets.Client
import qualified Network.WebSockets as WS
import           Control.Concurrent.STM.TChan (newTChan, readTChan, writeTChan)
import qualified Enecuum.Framework.Networking.Internal as Int 

-- | Interpret NetworkingL language.
interpretNetworkingL :: L.NetworkingF a -> IO a
interpretNetworkingL (L.SendRpcRequest (D.Address host port) request next) =
    runClient host (fromEnum port) "/" $ \connect -> do
        WS.sendTextData connect $ A.encode request
        next . transformEither T.pack id . A.eitherDecode <$> WS.receiveData connect

--
interpretNetworkingL (L.SendMessage conn msg next) = do
    Int.send conn msg
    pure $ next ()
interpretNetworkingL _ =
    error "interpretNetworkingL EvalNetwork not implemented."

transformEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
transformEither f _ (Left a)  = Left (f a)
transformEither _ f (Right a) = Right (f a)

-- | Run Networking language.
runNetworkingL :: L.NetworkingL a -> IO a
runNetworkingL = foldFree interpretNetworkingL
