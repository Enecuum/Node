module Enecuum.Framework.Networking.Interpreter where

import Enecuum.Prelude

import           Data.Aeson as A
import qualified Data.Text as T

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L
import           Enecuum.Legacy.Service.Network.WebSockets.Client
import qualified Network.WebSockets as WS
import qualified Enecuum.Framework.Networking.Internal as Int 
import           Enecuum.Framework.Runtime
import qualified Enecuum.Framework.RLens as RL

-- | Interpret NetworkingL language.
interpretNetworkingL :: NodeRuntime -> L.NetworkingF a -> IO a
interpretNetworkingL _ (L.SendRpcRequest (D.Address host port) request next) =
    runClient host (fromEnum port) "/" $ \connect -> do
        WS.sendTextData connect $ A.encode request
        next . transformEither T.pack id . A.eitherDecode <$> WS.receiveData connect

--
interpretNetworkingL nr (L.SendMessage (D.NetworkConnection conn) msg next) = do
    atomically $ do
        m <- readTVar $ nr ^. RL.connects
        whenJust (m ^. at conn) $ \con-> Int.send con msg
    pure $ next ()

interpretNetworkingL _  _ =
    error "interpretNetworkingL EvalNetwork not implemented."

transformEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
transformEither f _ (Left a)  = Left (f a)
transformEither _ f (Right a) = Right (f a)

-- | Run Networking language.
runNetworkingL ::  NodeRuntime -> L.NetworkingL a -> IO a
runNetworkingL nr = foldFree (interpretNetworkingL nr)
