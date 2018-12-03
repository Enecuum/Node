module Enecuum.Assets.Services.Pos.Handlers
    ( udpPosServiceHandlers
    , udpPosDummyService
    ) where

import           Enecuum.Prelude
import qualified Enecuum.Language                                           as L
import qualified Enecuum.Domain                                             as D

import           Enecuum.Assets.Services.Graph hiding (acceptKBlock)
import           Enecuum.Assets.Services.Routing

import           Enecuum.Assets.Services.Pos.RuntimeData
import           Enecuum.Assets.Services.Pos.Cryptography
import           Enecuum.Assets.Services.Pos.Messages
import           Enecuum.Assets.Services.Pos.TrinityData
import           Enecuum.Assets.Services.Pos.Types

udpPosServiceHandlers :: PosServiceRuntimeData -> L.NetworkHandlerL D.Udp L.NodeL ()
udpPosServiceHandlers posData = do
    let rData = _posRoutingRuntime posData
    L.handler $ udpBroadcastReceivedMessage rData (acceptNewKBlock          posData)
    L.handler $ udpForwardIfNeeded          rData (acceptUnsignedMicroBlock posData)
    L.handler $ udpBroadcastReceivedMessage rData (acceptMyRoleIs           posData)
    L.handler $ udpBroadcastReceivedMessage rData (acceptIAmNewPos          posData)
    L.handler $ udpForwardIfNeeded          rData (acceptIAmPos             posData)
    L.handler $ udpBroadcastReceivedMessage rData (acceptShadowRequest      posData)
    L.handler $ udpForwardIfNeeded          rData (acceptShadowResponce     posData)
    L.handler $ udpBroadcastReceivedMessage rData (\(_ :: LeaderBeacon) -> pure ())

-- accepting of key blocks
acceptNewKBlock :: PosServiceRuntimeData -> D.KBlock -> L.NodeL ()
acceptNewKBlock posData kBlock = do
    acceptKBlock' (_posGraphServiceData posData) kBlock
    entryToKBlockAcceptedStageIO posData kBlock

-- accepting of unsigned micro blocks
acceptUnsignedMicroBlock :: PosServiceRuntimeData -> UnsignedMicroblock -> L.NodeL ()
acceptUnsignedMicroBlock posData unsignedBlock =
    whenM (iAmPosLeader posData) $ 
        whenM (checkMicroBlock posData unsignedBlock) $
            void $ adoptUnsignedMicroBlock posData unsignedBlock

checkMicroBlock :: PosServiceRuntimeData -> UnsignedMicroblock -> L.NodeL Bool
checkMicroBlock _ _ = pure True 

adoptUnsignedMicroBlock :: PosServiceRuntimeData -> UnsignedMicroblock -> L.NodeL Bool
adoptUnsignedMicroBlock posData unsignedBlock = do
    mSignedBlock <- signMicroBlock posData unsignedBlock
    case mSignedBlock of
        Just mBlock -> do
            acceptMBlock'    (_posGraphServiceData posData) mBlock
            sendUdpBroadcast (_posRoutingRuntime posData)   mBlock
        Nothing -> pure False

-- accepting of msg from leader in pos voting
acceptMyRoleIs :: PosServiceRuntimeData -> MyRoleIs -> L.NodeL ()
acceptMyRoleIs posData message = L.atomically $
    whenM (senderIsCurrentPosLeader posData message) $
        whenM (isVotingStage posData) $
            entryToKeyGenerationStage posData PosCommon

senderIsCurrentPosLeader :: TrinityData a => a -> MyRoleIs -> L.StateL Bool
senderIsCurrentPosLeader posData (MyRoleIs role kHash _) = do
    ok <- isCurrentBlock posData kHash
    pure $ ok && role == PosLeader

-- addition of new pos to net
acceptIAmNewPos :: PosServiceRuntimeData -> IAmNewPos -> L.NodeL ()
acceptIAmNewPos posData (IAmNewPos posNodeId) = do
    addNewPos posData posNodeId
    iAmPos <- makeIAmNewPos posData
    udpMsgSending (_posRoutingRuntime posData) $ IAmPos posNodeId 64 iAmPos

acceptIAmPos :: TrinityData a => a -> IAmPos -> L.NodeL ()
acceptIAmPos posData (IAmPos _ _ (IAmNewPos posNodeId)) = addNewPos posData posNodeId

acceptShadowRequest :: PosServiceRuntimeData -> ShadowRequest -> L.NodeL ()
acceptShadowRequest posData (ShadowRequest kHash posNodeId) = do
    isPoeLeader <- isPosLeader posData posNodeId
    isCurrent   <- isCurrentBlockIO posData kHash
    when (isPoeLeader && isCurrent) $ do
        let myNodeId = getMyNodeId posData
        entryToKeyMadeСStageIO posData
        let shadowResponce = ShadowResponce posNodeId 64 myNodeId
        udpMsgSending (_posRoutingRuntime posData) shadowResponce

acceptShadowResponce :: PosServiceRuntimeData -> ShadowResponce -> L.NodeL ()
acceptShadowResponce posData (ShadowResponce _ _ nodeId ) = do
    L.atomically $ do 
        currentStage <- getStage posData
        when (currentStage == KeyGeneration PosLeader) $
            incrementKey posData nodeId ShadowKey
    whenJustM (getPosLeaderKeyIO posData) $ \_ -> do
        kBlock <- getCurrentKBlockIO posData
        void $ sendUdpBroadcast (_posRoutingRuntime posData) $
            LeaderBeacon (D.toHash kBlock) (getMyNodeId posData)

udpPosDummyService :: RoutingRuntime -> L.NetworkHandlerL D.Udp L.NodeL ()
udpPosDummyService routingData = do
    L.handler $ udpBroadcastReceivedMessage routingData (\(_ :: D.KBlock) -> pure ())
    L.handler $ udpBroadcastReceivedMessage routingData (\(_ :: MyRoleIs) -> pure ())
    L.handler $ udpBroadcastReceivedMessage routingData (\(_ :: IAmNewPos) -> pure ())
    L.handler $ udpForwardIfNeeded          routingData (\(_ :: IAmPos) -> pure ())
    L.handler $ udpBroadcastReceivedMessage routingData (\(_ :: ShadowRequest) -> pure ())
    L.handler $ udpForwardIfNeeded          routingData (\(_ :: ShadowResponce) -> pure ())
    L.handler $ udpBroadcastReceivedMessage routingData (\(_ :: LeaderBeacon) -> pure ())