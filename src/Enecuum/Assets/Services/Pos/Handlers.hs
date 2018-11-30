module Enecuum.Assets.Services.Pos.Handlers
    ( udpPosServiceHandlers
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
    L.handler $ acceptKBlock             posData
    L.handler $ acceptUnsignedMicroblock posData
    L.handler $ acceptMyRoleIs           posData
    L.handler $ acceptIAmNewPos          posData
    L.handler $ acceptIAmPos             posData
    L.handler $ acceptShadowRequest      posData
    L.handler $ acceptShadowResponce     posData

-- accepting of key blocks
acceptKBlock :: PosServiceRuntimeData -> D.KBlock -> D.Connection D.Udp -> L.NodeL ()
acceptKBlock posData = udpBroadcastReceivedMessage
    (_posRoutingRuntime posData)
    (acceptNewKBlock' posData)

acceptNewKBlock' :: PosServiceRuntimeData -> D.KBlock -> L.NodeL ()
acceptNewKBlock' posData kBlock = do
    acceptKBlock' (_posGraphServiceData posData) kBlock
    entryToKBlockAcceptedStageIO posData kBlock

-- accepting of unsigned micro blocks
acceptUnsignedMicroblock :: PosServiceRuntimeData ->  D.UnsignedMicroblock -> D.Connection D.Udp ->  L.NodeL ()
acceptUnsignedMicroblock posData unsignedBlock connection = do
    L.close connection
    acceptUnsignedMicroBlock' posData unsignedBlock 

acceptUnsignedMicroBlock' :: PosServiceRuntimeData -> D.UnsignedMicroblock -> L.NodeL ()
acceptUnsignedMicroBlock' posData unsignedBlock =
    whenM (iAmPosLeader posData) $ 
        whenM (checkMicroBlock posData unsignedBlock) $
            void $ adoptUnsignedMicroBlock posData unsignedBlock

checkMicroBlock :: PosServiceRuntimeData -> D.UnsignedMicroblock -> L.NodeL Bool
checkMicroBlock _ _ = pure True 

adoptUnsignedMicroBlock :: PosServiceRuntimeData -> D.UnsignedMicroblock -> L.NodeL Bool
adoptUnsignedMicroBlock posData unsignedBlock = do
    mSignedBlock <- signMicroBlock posData unsignedBlock
    case mSignedBlock of
        Just mBlock -> do
            acceptMBlock'    (_posGraphServiceData posData) mBlock
            sendUdpBroadcast (_posRoutingRuntime posData)   mBlock
        Nothing -> pure False

-- accepting of msg from leader in pos voting
acceptMyRoleIs :: PosServiceRuntimeData -> MyRoleIs -> D.Connection D.Udp -> L.NodeL ()
acceptMyRoleIs posData = udpBroadcastReceivedMessage
    (_posRoutingRuntime posData) (acceptMyRoleIs' posData)

acceptMyRoleIs' :: PosServiceRuntimeData -> MyRoleIs -> L.NodeL ()
acceptMyRoleIs' posData message = L.atomically $
    whenM (senderIsCurrentPosLeader posData message) $
        whenM (isVotingStage posData) $
            entryToKeyGenerationStage posData PosCommon

senderIsCurrentPosLeader :: TrinityData a => a -> MyRoleIs -> L.StateL Bool
senderIsCurrentPosLeader posData (MyRoleIs role kHash _) = do
    ok <- isCurrentBlock posData kHash
    pure $ ok && role == PosLeader

-- addition of new pos to net
acceptIAmNewPos :: PosServiceRuntimeData -> IAmNewPos -> D.Connection D.Udp -> L.NodeL ()
acceptIAmNewPos posData = udpBroadcastReceivedMessage
    (_posRoutingRuntime posData) (acceptIAmNewPos' posData)

acceptIAmNewPos' :: PosServiceRuntimeData -> IAmNewPos -> L.NodeL ()
acceptIAmNewPos' posData (IAmNewPos posNodeId) = do
    addNewPos posData posNodeId
    iAmPos <- makeIAmNewPos posData
    udpMsgSending (_posRoutingRuntime posData) $ IAmPos posNodeId 64 iAmPos

acceptIAmPos :: PosServiceRuntimeData -> IAmPos -> D.Connection D.Udp -> L.NodeL ()
acceptIAmPos posData = udpForwardIfNeeded
    (_posRoutingRuntime posData) (acceptIAmPos' posData)

acceptIAmPos' :: TrinityData a => a -> IAmPos -> L.NodeL ()
acceptIAmPos' posData (IAmPos _ _ (IAmNewPos posNodeId)) =
    addNewPos posData posNodeId

-- addition of new pos to net
acceptShadowRequest :: PosServiceRuntimeData -> ShadowRequest -> D.Connection D.Udp -> L.NodeL ()
acceptShadowRequest posData = udpBroadcastReceivedMessage
    (_posRoutingRuntime posData) (acceptShadowRequest' posData)

acceptShadowRequest' :: PosServiceRuntimeData -> ShadowRequest -> L.NodeL ()
acceptShadowRequest' posData (ShadowRequest kHash posNodeId) = do
    isPoeLeader <- isPosLeader posData posNodeId
    isCurrent   <- isCurrentBlockIO posData kHash
    when (isPoeLeader && isCurrent) $ do
        let myNodeId = getMyNodeId posData
        entryToKeyMadeСStageIO posData
        let shadowResponce = ShadowResponce posNodeId 64 myNodeId
        udpMsgSending (_posRoutingRuntime posData) shadowResponce

acceptShadowResponce :: PosServiceRuntimeData -> ShadowResponce -> D.Connection D.Udp -> L.NodeL ()
acceptShadowResponce posData = udpForwardIfNeeded
    (_posRoutingRuntime posData) (acceptShadowResponce' posData)

acceptShadowResponce' :: PosServiceRuntimeData -> ShadowResponce -> L.NodeL ()
acceptShadowResponce' posData (ShadowResponce _ _ nodeId ) = do
    L.atomically $ do 
        currentStage <- getStage posData
        when (currentStage == KeyGeneration PosLeader) $
            incrementKey posData nodeId ShadowKey
    whenJustM (getPosLeaderKeyIO posData) $ \_ -> do
        kBlock <- getCurrentKBlockIO posData
        void $ sendUdpBroadcast (_posRoutingRuntime posData) $
            LeaderBeacon (D.toHash kBlock) (getMyNodeId posData)

{-
    incrementKeyIO :: TrinityData a => a -> D.NodeId -> ShadowKey -> L.NodeL ()
incrementKeyIO posData nodeId sKey = L.atomically $
    incrementKey posData nodeId sKey 

getPosLeaderKeyIO :: TrinityData a => a -> L.NodeL (Maybe PosKey)
getPosLeaderKeyIO posData = L.atomically $ getPosLeaderKey posData
-}

{- TODO
udpPosServiceHandlers :: L.NetworkHandlerL D.Udp L.NodeL ()
udpPosDummyService = do
    L.handler $ acceptKBlock             
    L.handler $ acceptUnsignedMicroblock 
    L.handler $ acceptMyRoleIs           
    L.handler $ acceptIAmNewPos          
    L.handler $ acceptIAmPos             
    L.handler $ acceptShadowRequest      
    L.handler $ acceptShadowResponce     
-}