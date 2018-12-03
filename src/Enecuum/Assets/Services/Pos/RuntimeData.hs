module Enecuum.Assets.Services.Pos.RuntimeData
    ( PosServiceRuntimeData(..)
    , PosRole(..)
    , GetMyNodeId(..)
    , getDefultRole
    , getCurrentKBlock
    , getStage
    , entryToKBlockAcceptedStageIO
    , entryToKeyGenerationStage
    , makePosServiceData
    , iAmPosLeader
    , addNewPos
    , makeIAmNewPos
    ) where

import           Enecuum.Prelude
import qualified Enecuum.Language                                           as L
import qualified Enecuum.Domain                                             as D

import qualified Data.Set                                                   as S
import qualified Data.Map                                                   as M

import           Enecuum.Assets.Services.Graph
import           Enecuum.Assets.Services.Routing

import           Enecuum.Assets.Services.Pos.Config
import           Enecuum.Assets.Services.Pos.Types
import           Enecuum.Assets.Services.Pos.TrinityData 
import           Enecuum.Assets.Services.Pos.Messages

data PosServiceRuntimeData = PosServiceRuntimeData
    { _posRoutingRuntime    :: RoutingRuntime
    , _posGraphServiceData  :: GraphServiceData
    , _posTrinityData       :: PosTrinityData
    }

makePosServiceData :: PosConfig -> RoutingRuntime -> GraphServiceData -> L.NodeDefinitionL PosServiceRuntimeData
makePosServiceData posConfif routingData graphData = do
    posTrinityData <- makePosTrinityData posConfif
    pure $ PosServiceRuntimeData routingData graphData posTrinityData

--
instance TrinityData PosServiceRuntimeData where
    toTrinityData = _posTrinityData

makeIAmNewPos :: PosServiceRuntimeData -> L.NodeL IAmNewPos
makeIAmNewPos posData = pure $ IAmNewPos $ getMyNodeId posData

instance GetMyNodeId PosServiceRuntimeData where
    getMyNodeId = getMyNodeId . _posRoutingRuntime