module Enecuum.Framework.Networking.Interpreter where

import Enecuum.Prelude

import           Data.Aeson as A
import qualified Data.Text as T

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L
import qualified Enecuum.Framework.Networking.Internal.Internal as Int 
import           Enecuum.Framework.Runtime
import qualified Enecuum.Framework.RLens as RL
import qualified Network.Socket.ByteString.Lazy     as S
import qualified Enecuum.Framework.Networking.Internal.TCP.Client as TCP

-- | Interpret NetworkingL language.
interpretNetworkingL :: NodeRuntime -> L.NetworkingF a -> IO a
interpretNetworkingL _ (L.SendRpcRequest (D.Address host port) request next) = do
    var <- newEmptyMVar
    ok <- try $ TCP.runClient host port $ \connect -> do
        S.sendAll connect $ A.encode request
        msg <- S.recv connect (1024 * 4)
        putMVar var (transformEither T.pack id $ A.eitherDecode msg)
    case ok of
        Right _ -> pure ()
        Left (_ :: SomeException) -> putMVar var $ Left "Server size does not exist."
    res <- takeMVar var
    return $ next res

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
