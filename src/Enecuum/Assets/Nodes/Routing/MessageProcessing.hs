module Enecuum.Assets.Nodes.Routing.MessageProcessing where

import qualified Enecuum.Assets.Nodes.Address     as A
import qualified Enecuum.Assets.Nodes.Messages    as M
import           Enecuum.Assets.Nodes.Methods
import qualified Enecuum.Domain                   as D
import qualified Enecuum.Language                 as L
import           Enecuum.Prelude
import           Enecuum.Research.ChordRouteMap
import           Enecuum.Assets.Nodes.Routing.Messages
import qualified Data.Sequence as Seq
import qualified Data.Set      as Set
import           Enecuum.Assets.Nodes.Routing.RuntimeData
import           Enecuum.Assets.Nodes.Routing.RoutingWorker

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
    => HasNodeReciverId message D.StringHash
    => RoutingRuntime -> message -> (message -> L.NodeL ()) -> L.NodeL ()
udpForwardIfNeeded routingRuntime message handler
    -- process message if I am a recipient
    | routingRuntime ^. myNodeAddres . A.nodeId == message ^. nodeReciverId = handler message
    -- forward the message further if it is not yet old.
    | message ^. timeToLive > 0 = 
        udpMsgSending routingRuntime (message & timeToLive %~ (\x -> x - 1))
    -- drop message if it is too old
    | otherwise = pure ()


-- send usp msg to node
udpMsgSending routingRuntime message = do
    let reciverId = message ^. nodeReciverId
    L.logInfo $ "Resending to " <> show reciverId
    connects <- L.readVarIO (routingRuntime ^. connectMap)
    let nextReciver = findNextResender reciverId connects
    whenJust nextReciver $ \(_, address) ->
        void $ L.notify (A.getUdpAddress address) message
    unless (isJust nextReciver) $ L.logError "Connection map is empty. Fail of resending."

-- forward and proccessing the received message if necessary
udpBroadcastRecivedMessage
    :: (ToJSON msg, Typeable msg, Serialize msg)
    => RoutingRuntime -> (msg -> L.NodeL ()) -> msg ->  D.Connection D.Udp -> L.NodeL ()
udpBroadcastRecivedMessage routingRuntime handler message conn = do
    L.close conn
    -- it is possible to forward only new messages
    -- if the message did not broadcasted, then we have already processed it
    whenM (sendUdpBroadcast routingRuntime message) $ handler message
