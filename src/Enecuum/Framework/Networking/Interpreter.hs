module Enecuum.Framework.Networking.Interpreter where

import Enecuum.Prelude

import           Data.Aeson as A
import qualified Data.Text as T

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L
import qualified Enecuum.Framework.Networking.Internal.Tcp.Connection as Tcp
import qualified Enecuum.Framework.Networking.Internal.Udp.Connection as Udp
import qualified Enecuum.Framework.Networking.Internal.Connection     as Con
import           Enecuum.Framework.Runtime
import qualified Enecuum.Framework.RLens as RL
import qualified Network.Socket.ByteString.Lazy     as S
import           Enecuum.Framework.Networking.Internal.Client

-- | Interpret NetworkingL language.
interpretNetworkingL :: NodeRuntime -> L.NetworkingF a -> IO a
interpretNetworkingL _ (L.SendRpcRequest addr request next) = do
    var <- newEmptyMVar
    ok  <- try $ runClient D.TCP addr $ \connect -> do
        S.sendAll connect $ A.encode request
        msg <- S.recv connect (1024 * 4)
        putMVar var (transformEither T.pack id $ A.eitherDecode msg)
    case ok of
        Right _                    -> pure ()
        Left  (_ :: SomeException) -> putMVar var $ Left "Server size does not exist."
    res <- takeMVar var
    pure $ next res

interpretNetworkingL nr (L.SendTcpMsgByConnection (D.Connection conn) msg next) = do
    m <- atomically $ readTVar $ nr ^. RL.tcpConnects
    case m ^. at conn of
        Just con -> next <$> Con.send con msg
        Nothing  -> pure $ next $ Left "The connection is closed."

interpretNetworkingL nr (L.SendUdpMsgByConnection (D.Connection conn) msg next) = do
    m <- atomically $ readTVar $ nr ^. RL.udpConnects
    case m ^. at conn of
        Just con -> next <$> Con.send con msg
        Nothing  -> pure $ next $ Left "The connection is closed."

interpretNetworkingL _ (L.SendUdpMsgByAddress adr msg next) =
    next <$> Udp.sendUdpMsg adr msg

interpretNetworkingL _ _ = error "interpretNetworkingL EvalNetwork not implemented."

transformEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
transformEither f _ (Left  a) = Left (f a)
transformEither _ f (Right a) = Right (f a)

-- | Run Networking language.
runNetworkingL :: NodeRuntime -> L.NetworkingL a -> IO a
runNetworkingL nr = foldFree (interpretNetworkingL nr)
