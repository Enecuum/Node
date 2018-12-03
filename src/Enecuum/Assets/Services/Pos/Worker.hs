module Enecuum.Assets.Services.Pos.Worker (runPosService) where

import           Enecuum.Prelude
import qualified Enecuum.Language                                           as L
import qualified Enecuum.Domain                                             as D

import           Enecuum.Assets.Services.Graph
import           Enecuum.Assets.Services.Routing

import           Enecuum.Assets.Services.Pos.RuntimeData
import           Enecuum.Assets.Services.Pos.Messages
import           Enecuum.Assets.Services.Pos.Config
import           Enecuum.Assets.Services.Pos.Types
import           Enecuum.Assets.Services.Pos.TrinityData

runPosService :: PosConfig -> RoutingRuntime -> GraphServiceData -> L.NodeDefinitionL PosServiceRuntimeData 
runPosService posConfig routingRuntimeData graphServiceData = do
    posData <- makePosServiceData posConfig routingRuntimeData graphServiceData
    L.process $ runPosWorker posData
    sendToAllIAmPos posData
    pure posData

runPosWorker :: PosServiceRuntimeData -> L.NodeL ()
runPosWorker posData = forever $ do
    kBlock <- waitNewKeyBlock posData
    posVoting                 posData kBlock
    keyGeneration             posData

waitNewKeyBlock :: PosServiceRuntimeData -> L.NodeL D.KBlock
waitNewKeyBlock posData = L.atomically $ do
    stage <- getStage posData
    unless (stage == KBlockAccepted) L.retry
    getCurrentKBlock posData

-- TODO: voting procedure
posVoting :: PosServiceRuntimeData -> D.KBlock -> L.NodeL ()
posVoting posData kBlock =
    whenJust (getDefultRole posData) $ \role -> do
        L.atomically $ entryToKeyGenerationStage posData role
        when (role == PosLeader) $ do
            iAmLeaderMessage <- makeIAmLeaderMessage posData kBlock
            void $ sendUdpBroadcast (_posRoutingRuntime posData) iAmLeaderMessage

makeIAmLeaderMessage :: PosServiceRuntimeData -> D.KBlock -> L.NodeL MyRoleIs
makeIAmLeaderMessage posData kBlock =
    pure $ MyRoleIs PosLeader (D.toHash kBlock) $ getMyNodeId posData

sendToAllIAmPos :: PosServiceRuntimeData -> L.NodeDefinitionL ()
sendToAllIAmPos posData = L.scenario $ do
    iAmNewPos <- makeIAmNewPos posData
    void $ sendUdpBroadcast (_posRoutingRuntime posData) iAmNewPos

keyGeneration :: PosServiceRuntimeData -> L.NodeL ()
keyGeneration posData = whenM (iAmPosLeader posData) $ do
    currentKHash <- D.toHash <$> getCurrentKBlockIO posData
    let myNodeId  = getMyNodeId posData
    let shadowReq = ShadowRequest currentKHash myNodeId
    void $ sendUdpBroadcast (_posRoutingRuntime posData) shadowReq
