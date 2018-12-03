module Enecuum.Assets.Services.Pos.TrinityData where

import           Enecuum.Prelude
import qualified Enecuum.Language                                           as L
import qualified Enecuum.Domain                                             as D

import qualified Data.Set                                                   as S
import qualified Data.Map                                                   as M

import           Enecuum.Assets.Services.Pos.Config
import           Enecuum.Assets.Services.Pos.Types

data PosTrinityData = PosTrinityData
    { _posTrinityConfig :: PosTrinityConfig
    , _posStageData     :: D.StateVar PosStageData
    , _posStage         :: D.StateVar TrinityStage
    , _posList          :: D.StateVar (S.Set D.NodeId)
    }

makePosTrinityData :: PosConfig -> L.NodeDefinitionL PosTrinityData
makePosTrinityData posConfif = do
    posStageData <- L.newVarIO PosInitStageData
    posStage     <- L.newVarIO PosInitStage
    posList      <- L.newVarIO mempty
    pure $ PosTrinityData (makePosTrinityConfig posConfif) posStageData posStage posList

newtype PosTrinityConfig = PosTrinityConfig
    { _posDefaultRole       :: Maybe PosRole
    }

makePosTrinityConfig :: PosConfig -> PosTrinityConfig
makePosTrinityConfig posConfif = PosTrinityConfig $ Just $ _posDefRole posConfif

data PosStageData
    = PosInitStageData

    | KBlockAcceptedData
    { _currentKBlock :: D.KBlock
    }

    | PosVotingData
    { _currentKBlock :: D.KBlock
    }

    | KeyGenerationDataL
    { _currentKBlock :: D.KBlock
    , _keys          :: M.Map D.NodeId ShadowKey
    }

    | KeyGenerationDataC
    { _currentKBlock :: D.KBlock
    }

    | KeyMadeDataL
    { _currentKBlock :: D.KBlock
    , _posKey        :: PosKey
    }

    | KeyMadeDataC
    { _currentKBlock :: D.KBlock
    }

toKBlockAcceptedData :: D.KBlock -> PosStageData -> PosStageData
toKBlockAcceptedData kBlock _ = KBlockAcceptedData kBlock


assert text assertion = unlessM assertion $ L.logError text

class TrinityData a where
    toTrinityData :: a -> PosTrinityData

    setStage :: a -> TrinityStage -> L.StateL ()
    setStage posData = setStage (toTrinityData posData)

    getStage :: a -> L.StateL TrinityStage
    getStage posData = getStage (toTrinityData posData)

    addNewPos :: a -> D.NodeId -> L.NodeL ()
    addNewPos posData = addNewPos (toTrinityData posData)

    getDefultRole :: a -> Maybe PosRole
    getDefultRole posData = getDefultRole (toTrinityData posData)

    getCurrentKBlock :: a -> L.StateL D.KBlock
    getCurrentKBlock posData = getCurrentKBlock (toTrinityData posData)

    entryToKBlockAcceptedStage :: a -> D.KBlock -> L.StateL ()
    entryToKBlockAcceptedStage posData = entryToKBlockAcceptedStage (toTrinityData posData)

    entryToKeyGenerationStage :: a -> PosRole -> L.StateL ()
    entryToKeyGenerationStage posData = entryToKeyGenerationStage (toTrinityData posData)

    entryToKeyMadeСStage :: a -> L.StateL ()
    entryToKeyMadeСStage posData = entryToKeyMadeСStage (toTrinityData posData)

    incrementKey :: a -> D.NodeId -> ShadowKey -> L.StateL ()
    incrementKey posData = incrementKey (toTrinityData posData)
    
    getPosLeaderKey :: a -> L.StateL (Maybe PosKey)
    getPosLeaderKey posData = getPosLeaderKey (toTrinityData posData)

instance TrinityData PosTrinityData where
    toTrinityData    = id
    setStage posData = L.writeVar (_posStage posData)
    getStage posData = L.readVar  (_posStage posData)
    
    addNewPos posData newPosId =
        L.modifyVarIO (_posList posData) (S.insert newPosId)

    getDefultRole = _posDefaultRole . _posTrinityConfig

    getCurrentKBlock posData = do
        stage <- getStage posData
        when (stage == PosInitStage) L.retry
        _currentKBlock <$> L.readVar (_posStageData posData)
    
    entryToKeyGenerationStage posData role = do
        assert
            ("The transition to the state “KeyGeneration” should be" <>
             " made only from state “PosVoting”") $ do
                stage <- getStage posData
                pure $ stage == PosVoting
     
        let transform (PosVotingData kBlock) = case role of
                PosLeader -> KeyGenerationDataL kBlock mempty
                PosCommon -> KeyGenerationDataC kBlock
            transform _ = error "It not be able!"
        L.modifyVar (_posStageData posData) transform
        setStage posData (KeyGeneration role)

    entryToKeyMadeСStage posData = do
        assert
            ("The transition to the state “KeyMade” should be" <>
            " made only from state “KeyGeneration”") $ do
                stage <- getStage posData
                pure $ stage == KeyGeneration PosCommon
        let transform (KeyGenerationDataC kBlock) = KeyMadeDataC kBlock
            transform _ = error "It not be able!"
        L.modifyVar (_posStageData posData) transform
        setStage posData (KeyMade PosCommon)

    entryToKBlockAcceptedStage posData kBlock = do
        L.modifyVar (_posStageData posData) (toKBlockAcceptedData kBlock)
        setStage posData KBlockAccepted
        
    incrementKey posData nodeId key = do
        assert "Only a pos leader in state KeyGeneration can “incrementKey“" $ do
            stage <- getStage posData
            pure $ stage == KeyGeneration PosLeader
        stage <- getStage posData
        when (stage == KeyGeneration PosLeader) $ do
            KeyGenerationDataL kBlock posKeys <- L.readVar (_posStageData posData)
            L.writeVar (_posStageData posData) $ KeyGenerationDataL kBlock (M.insert nodeId key posKeys)

    getPosLeaderKey posData = do
        stage <- getStage posData
        guardJ (stage == KeyGeneration PosLeader) $ do
            KeyGenerationDataL _ posKeys <- L.readVar (_posStageData posData)
            posNumber <- S.size <$> L.readVar (_posList posData)
            guardJ ((1000 * posNumber) `div` (1000 * M.size posKeys) > 500) $
                pure $ Just PosKey


guardJ :: Applicative f => Bool -> f (Maybe a) -> f (Maybe a)
guardJ ok just = if ok then just else pure Nothing

getStageIO :: TrinityData a => a -> L.NodeL TrinityStage
getStageIO = L.atomically . getStage

getCurrentKBlockIO :: TrinityData a => a -> L.NodeL D.KBlock
getCurrentKBlockIO posData = L.atomically $ getCurrentKBlock posData

isVotingStage :: TrinityData a => a -> L.StateL Bool
isVotingStage posData = do
    stage <- getStage posData
    pure $ stage == PosVoting

entryToKBlockAcceptedStageIO :: TrinityData a => a -> D.KBlock -> L.NodeL ()
entryToKBlockAcceptedStageIO posData kBlock =
    L.atomically $ entryToKBlockAcceptedStage posData kBlock

iAmPosLeader :: TrinityData a => a -> L.NodeL Bool
iAmPosLeader posData = do
    posStage <- getStageIO posData
    pure $ posStage `elem` [KeyGeneration PosLeader, KeyMade PosLeader]

isPosLeader :: TrinityData a => a -> D.NodeId -> L.NodeL Bool
isPosLeader _ _ = pure True

isCurrentBlock :: TrinityData a => a -> D.StringHash -> L.StateL Bool
isCurrentBlock posData kHash = do
    kBlock <- getCurrentKBlock posData
    pure $ D.toHash kBlock == kHash

isCurrentBlockIO :: TrinityData a => a -> D.StringHash -> L.NodeL Bool
isCurrentBlockIO posData kHash = L.atomically $ isCurrentBlock posData kHash 

entryToKeyMadeСStageIO :: TrinityData a => a -> L.NodeL ()
entryToKeyMadeСStageIO posData = L.atomically $ entryToKeyMadeСStage posData

incrementKeyIO :: TrinityData a => a -> D.NodeId -> ShadowKey -> L.NodeL ()
incrementKeyIO posData nodeId sKey = L.atomically $
    incrementKey posData nodeId sKey 

getPosLeaderKeyIO :: TrinityData a => a -> L.NodeL (Maybe PosKey)
getPosLeaderKeyIO posData = L.atomically $ getPosLeaderKey posData
