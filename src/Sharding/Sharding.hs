{-# LANGUAGE
        LambdaCase
    ,   MultiWayIf
    ,   ViewPatterns
#-}

-- 1. Request of block
-- -> NetLvl -> LogicLvl -> NetLvl ->
--              | <---------------------------+
--              |                             |
--    toStore --+-- To NetLvl -> LogicLvl -> NetLvl

-- -> NetLevet: BlockRequest
-- NetLevet -> LogicLvl: BlockRequest (Chan for Response) BlockHash
-- LogicLvl -> NetLevet: BlockResponse (Maybe Block)
    -- LogicLvl (local) -> LogicLvl (non local): BlockRequest (Maybe Block)
    -- LogicLvl (non local) -> LogicLvl (local): BlockResponse (Maybe Block)

--- broadcastLvl                             logicLvl
---                   index shard
---                   store shard

-- mackroblock ([hash mickroblock] , sign, hash, hask prev)
-- epoch [hash mackroblock , sign, hash, hask prev]

-- hass -> microblock | head of mackroblock


--      logicLvl            logicLvl
--         |                   ^
--         V                   |
--      netLvl    ->  ->    netLvl
--
--   2 request with id 1 -> node -> block 1


module Sharding.Sharding where

import              Sharding.Space.Distance
import              Sharding.Space.Point
import              Sharding.Space.Shift
import              Sharding.Types.ShardTypes
import              Sharding.Types.ShardLogic
import              Sharding.Types.Node
import              Sharding.ShardDB.ShardIndex
import              Sharding.ShardDB.ShardStore

import qualified    Node.Node.Types     as T
import              Control.Concurrent.Chan
import              Data.List.Extra
import              Control.Concurrent
import              Lens.Micro
import              Lens.Micro.GHC
import              Control.Monad
import              Data.Word
import              Service.Timer
import qualified    Data.Set            as S
import qualified    Data.Map            as M
import              System.Clock

import              Node.Data.NodeTypes


--makeShardingNode :: MyNodeId -> Point -> IO ()
makeShardingNode aMyNodeId aChanRequest aChanOfNetLevel aMyNodePosition = do
    aShardingNode <- initOfShardingNode aChanOfNetLevel aChanRequest aMyNodeId aMyNodePosition
    void $ forkIO $ aLoop aShardingNode
  where
    aLoop :: ShardingNode -> IO ()
    aLoop aShardingNode = readChan aChanRequest >>= \case
        ShiftAction | shiftIsNeed aShardingNode ->
            shiftTheShardingNode aChanOfNetLevel aLoop aShardingNode

        TheNodeHaveNewCoordinates aNodeId aNodePosition
            | isInNodeDomain aShardingNode aNodePosition -> aLoop
                $ insertTheNeighbor aNodeId aNodePosition
                $ deleteTheNeighbor aNodeId aShardingNode
            | otherwise -> aLoop
                $ deleteTheNeighbor aNodeId aShardingNode

        TheNodeIsDead aNodeId -> aLoop
            $ deleteTheNeighbor aNodeId aShardingNode

        NewNodeInNetAction aNodeId aNodePosition -> aLoop
            $ insertTheNeighbor aNodeId aNodePosition aShardingNode

        ShardRequestAction  aShardHash aChan -> do
            aMaybeShard <- loadShard aShardHash
            case aMaybeShard of
                Just aShard -> do
                    writeChan aChan aShard
                    aLoop aShardingNode
                Nothing -> do
                    sendToNetLevet aChanOfNetLevel $ ShardListRequest [aShardHash]
                    aTime <- getTime Realtime
                    aLoop $ aShardingNode & nodeIndexOfReques %~
                        M.insert aShardHash (aTime, aChan)

        ShardIndexAcceptAction aShardHashs -> aLoop
            $ addShardingIndex (S.fromList aShardHashs) aShardingNode

        ShardIndexCreateAction aChan aNodeId aRadiusOfCapture -> do                         --- !!!!! TODO
            createShardingIndex aChan aShardingNode aNodeId aRadiusOfCapture
            aLoop aShardingNode

        ShardAcceptAction aShard
            | Just (_, aChan) <- aShardingNode^.nodeIndexOfReques.at (shardToHash aShard) -> do
                writeChan aChan aShard
                aLoop $ aShardingNode & nodeIndexOfReques %~ M.delete (shardToHash aShard)

            | checkShardIsInRadiusOfCaptureShardingNode aShardingNode (shardToHash aShard) ->
                nodeSaveShard aShard aLoop aShardingNode

        NewShardInNetAction aShard
            | checkShardIsInRadiusOfCaptureShardingNode aShardingNode (shardToHash aShard) ->
                nodeSaveShard aShard aLoop aShardingNode

        ShardListCreateAction aChan aNodeId aHashList -> do
            sendShardsToNode aShardingNode aNodeId aHashList aChan
            aLoop aShardingNode

        NodePositionAction aChan aNodeId -> do
            writeChan aChan $ NodePositionResponse (aShardingNode^.nodePosition)
            aLoop aShardingNode

        a -> error $ "Sharding.Sharding.makeShardingNode"


--------------------------------------------------------------------------------
--                              INTERNAL                                      --
--------------------------------------------------------------------------------
initOfShardingNode aChanOfNetLevel aChanRequest aMyNodeId aMyNodePosition = do
    sendToNetLevet aChanOfNetLevel $ IamAwakeRequst aMyNodeId aMyNodePosition

    aMyShardsIndex <- loadMyShardIndex

    metronome (10^8) $ do
        writeChan aChanRequest CleanShardsAction

    metronome (10^8) $ do
        writeChan aChanRequest ShiftAction

    return $ makeEmptyShardingNode S.empty aMyNodeId aMyNodePosition aMyShardsIndex


shiftTheShardingNode :: T.ManagerMsg msg =>
        Chan msg
    -> (ShardingNode ->  IO ())
    ->  ShardingNode
    ->  IO ()
shiftTheShardingNode aChanOfNetLevel aLoop aShardingNode = do
    let
        aNeighborPositions :: S.Set NodePosition
        aNeighborPositions = neighborPositions aShardingNode

        aMyNodePosition :: MyNodePosition
        aMyNodePosition    = aShardingNode^.nodePosition

        aNearestPositions :: S.Set NodePosition
        aNearestPositions  = S.fromList $
            findNearestNeighborPositions aMyNodePosition aNeighborPositions

        aNewPosition :: MyNodePosition
        aNewPosition       = shiftToCenterOfMass aMyNodePosition aNearestPositions

    sendToNetLevet aChanOfNetLevel $ NewPosiotionMsg aNewPosition
    aLoop $ aShardingNode & nodePosition .~ aNewPosition


deleteTheNeighbor :: NodeId -> ShardingNode -> ShardingNode
deleteTheNeighbor aNodeId aShardingNode =
    aShardingNode & nodeNeighbors %~ S.filter (\n -> n^.neighborId /= aNodeId)


insertTheNeighbor :: NodeId -> NodePosition -> ShardingNode -> ShardingNode
insertTheNeighbor aNodeId aNodePosition aShardingNode =
    aShardingNode & nodeNeighbors %~ S.insert (Neighbor aNodePosition aNodeId)


isInNodeDomain :: ShardingNode -> NodePosition -> Bool
isInNodeDomain aShardingNode aNodePosition =
    distanceTo (aShardingNode^.nodePosition) aNodePosition `div` neighborsMemoryDistanse < findShardingNodeDomain aShardingNode


addShardingIndex :: S.Set ShardHash ->  ShardingNode -> ShardingNode -- Is it one list or many?
addShardingIndex aShardIndex aShardingNode =
    aShardingNode & nodeIndex.shardNeededIndex.setOfHash %~ S.union aNeeded
  where
    aNeeded :: S.Set ShardHash
    aNeeded = S.difference aShardIndex aExistShards
    aExistShards = indexToSet (aShardingNode^.nodeIndex)


createShardingIndex :: Chan ShardingNodeResponce -> ShardingNode -> NodeId -> Word64 ->  IO ()
createShardingIndex aChanOfNetLevel aShardingNode aNodeId aRadiusOfCapture = do
    let aMaybeNeighbor = S.toList $ S.filter (\n -> n^.neighborId == aNodeId) $
            aShardingNode^.nodeNeighbors
    case aMaybeNeighbor of
        [Neighbor aNeighborPosition _] -> do
            let resultsShardingHash = S.filter
                    (checkShardIsInRadiusOfCapture aNeighborPosition aRadiusOfCapture)
                    (indexToSet $ aShardingNode^.nodeIndex)
            writeChan aChanOfNetLevel (ShardIndexResponse $ S.toList resultsShardingHash)
        _  -> do
            writeChan aChanOfNetLevel (ShardIndexResponse [])


checkShardIsInRadiusOfCaptureShardingNode :: ShardingNode -> ShardHash -> Bool
checkShardIsInRadiusOfCaptureShardingNode aShardNode aShardHash =
    checkShardIsInRadiusOfCapture
        (toNodePosition $ aShardNode^.nodePosition)
        (mul    (findShardingNodeDomain aShardNode)
                (distanceNormalizedCapture + aShardNode^.nodeDistance))
        aShardHash


sendShardsToNode ::
        ShardingNode
    ->  NodeId
    ->  ShardHash
    ->  Chan ShardingNodeResponce
    ->  IO ()
sendShardsToNode aShardingNode aNodeId aHashList aChanOfNetLevel = do
    [aShards] <- loadShards [aHashList]
    writeChan aChanOfNetLevel $ ShardResponse aShards

--------------------------------------------------------------------------------
--------------------------TODO-TO-REMOVE----------------------------------------
findNodeDomain :: MyNodePosition -> S.Set NodePosition -> Distance Point
findNodeDomain aMyPosition aPositions = if
    | length aNearestPoints < 4 -> maxBound
    | otherwise                 ->
        last . sort $ distanceTo aMyPosition <$> aNearestPoints
  where
    aNearestPoints = findNearestNeighborPositions aMyPosition aPositions


nodeSaveShard :: Shard -> (ShardingNode ->  IO ()) -> ShardingNode -> IO ()
nodeSaveShard aShard aLoop aShardingNode = do
    saveShard aShard
    aLoop $ aShardingNode & nodeIndex %~
        addShardToIndex aShard (aShardingNode^.nodePosition)


neighborPositions :: ShardingNode -> S.Set NodePosition
neighborPositions = S.map (^.neighborPosition) . (^.nodeNeighbors)


-- !!!! -> ????????????
findShardingNodeDomain :: ShardingNode -> Distance Point
findShardingNodeDomain aShardingNode = findNodeDomain
    (aShardingNode^.nodePosition)
    (neighborPositions aShardingNode)


shiftIsNeed :: ShardingNode -> Bool
shiftIsNeed aShardingNode = checkUnevenness
    (aShardingNode^.nodePosition) (neighborPositions aShardingNode)


sendToNetLevet :: T.ManagerMsg msg => Chan msg -> ShardingNodeRequestMsg -> IO ()
sendToNetLevet aChan aMsg = writeChan aChan $ T.shardingNodeRequestMsg aMsg


mul :: Word64 -> Word64 -> Word64
mul x y
    | aResult > toInteger (maxBound :: Word64) = maxBound
    | otherwise = fromInteger aResult
  where
    aResult = (toInteger x * toInteger y) `div` distanceNormalizedCapture
