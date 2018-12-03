module Enecuum.Assets.Services.Routing.MessageProcessing where

import qualified Enecuum.Assets.Nodes.Address             as A
import qualified Enecuum.Assets.Nodes.Messages            as M
import           Enecuum.Assets.Nodes.Methods
import           Enecuum.Assets.Services.Routing.Messages
import qualified Enecuum.Domain                           as D
import qualified Enecuum.Framework.Lens                   as Lens
import qualified Enecuum.Language                         as L
import           Enecuum.Prelude
import           Enecuum.Research.ChordRouteMap

import           Enecuum.Assets.Services.Routing.RuntimeData

-- | Send udp broadcast if the message is new.
sendUdpBroadcast
    :: Typeable message
    => ToJSON message
    => Serialize message
    => RoutingRuntime -> message -> L.NodeL Bool
sendUdpBroadcast routingRuntime message = do
    -- check if the message is familiar
    -- if not, add to the list of familiar messages
    needToBroadcast <- L.atomically $ do
        let messageHash = D.toHashGeneric message
        familiarMessage <- isInFilter routingRuntime messageHash
        unless familiarMessage $ registerMsg routingRuntime messageHash
        pure $ not familiarMessage

    -- resending of message to everyone we know
    when needToBroadcast $ do
        connectsToResending <- fromChordRouteMap <$> L.readVarIO (routingRuntime ^. connectMap)
        forM_ connectsToResending $ \receiverAddress ->
            void $ L.notify (A.getUdpAddress . snd $ receiverAddress) message
    pure needToBroadcast

-- | Forward and after proccesing udp message if it needed
udpForwardIfNeeded
    :: HasTimeToLive message Int
    => ToJSON message
    => Typeable message
    => HasNodeReceiverId message D.StringHash
    => RoutingRuntime -> (message -> L.NodeL ()) -> message -> D.Connection D.Udp -> L.NodeL ()
udpForwardIfNeeded routingRuntime handler message connection
    -- process message if I am a recipient
    | getMyNodeId routingRuntime == message ^. nodeReceiverId = do
        L.close connection
        handler message
    -- forward the message further if it is not yet old.
    | message ^. timeToLive > 0 = do
        L.close connection
        udpMsgSending routingRuntime (message & timeToLive %~ (\x -> x - 1))
    -- drop message if it is too old
    | otherwise = L.close connection


-- send usp msg to node
udpMsgSending :: (ToJSON a, Typeable a, L.SendUdp m,
                HasConnectMap s (D.StateVar (ChordRouteMap D.NodeAddress)),
                L.StateIO m, L.Logger m, Monad m,
                HasNodeReceiverId a D.StringHash) =>
                s -> a -> m ()
udpMsgSending routingRuntime message = do
    let receiverId = message ^. nodeReceiverId
    L.logInfo $ "Resending to " <> show receiverId
    connects <- L.readVarIO (routingRuntime ^. connectMap)
    let nextReceiver = findNextResender receiverId connects
    whenJust nextReceiver $ \(_, address) ->
        void $ L.notify (A.getUdpAddress address) message
    unless (isJust nextReceiver) $ L.logError "Connection map is empty. Fail of resending."

-- forward and proccessing the received message if necessary
udpBroadcastReceivedMessage
    :: (ToJSON msg, Typeable msg, Serialize msg)
    => RoutingRuntime -> (msg -> L.NodeL ()) -> msg -> D.Connection D.Udp -> L.NodeL ()
udpBroadcastReceivedMessage routingRuntime handler message conn = do
    L.close conn
    -- it is possible to forward only new messages
    -- if the message did not broadcasted, then we have already processed it
    whenM (sendUdpBroadcast routingRuntime message) $ handler message
