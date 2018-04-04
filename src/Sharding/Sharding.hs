{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
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
import              Control.Monad
import qualified    Data.ByteString     as B
import              Data.Word
import              Node.Data.Data
import              Service.Timer
import qualified    Data.Set            as S
import qualified    Data.Map            as M

import              Node.Data.NodeTypes
import              Node.Data.NetPackage
import              Node.Data.NetMesseges


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

        ShardIndexAcceptAction aShardHashs -> aLoop
            $ addShardingIndex (S.fromList aShardHashs) aShardingNode

        ShardIndexCreateAction aChan aNodeId aRadiusOfCapture -> do                         --- !!!!! TODO
            createShardingIndex aChan aShardingNode aNodeId aRadiusOfCapture
            aLoop aShardingNode

        ShardAcceptAction aShard
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

--        CleanShardsAction -> do

        _ -> undefined



--------------------------------------------------------------------------------
--                              INTERNAL                                      --
--------------------------------------------------------------------------------
initOfShardingNode aChanOfNetLevel aChanRequest aMyNodeId aMyNodePosition = do
    sendToNetLevet aChanOfNetLevel $ IamAwakeRequst aMyNodeId aMyNodePosition

    aMyShardsIndex <- loadMyShardIndex
    (aMyNeighbors, aMyPosition) <- loadInitInformation

    metronome (10^8) $ do
        writeChan aChanRequest CleanShardsAction

    metronome (10^8) $ do
        writeChan aChanRequest ShiftAction

    return $ makeEmptyShardingNode aMyNeighbors aMyNodeId aMyPosition aMyShardsIndex


shiftTheShardingNode ::
        Chan T.ManagerMiningMsgBase
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
addShardingIndex aShardIndex aShardingNode = undefined
    --aShardingNode & nodeIndex %~ S.union aShardIndex


createShardingIndex :: Chan ShardingNodeResponce -> ShardingNode -> NodeId -> Word64 ->  IO ()
createShardingIndex aChanOfNetLevel aShardingNode aNodeId aRadiusOfCapture = undefined
{-
 do
    let maybeNeighbor = S.toList$ S.filter (\n -> n^.neighborId == aNodeId) $
            aShardingNode^.nodeNeighbors
    case maybeNeighbor of
        [Neighbor aNeighborPosition _] -> do
            let resultsShardingHash = S.filter
                    (checkShardIsInRadiusOfCapture aNeighborPosition aRadiusOfCapture)
                    (aShardingNode^.nodeIndex)
            sendToNetLevet aChanOfNetLevel (ShardIndexResponse aNodeId $ S.toList resultsShardingHash)
        _                      -> return ()
-}
---- TODO after add db



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


-- TODO Is it file or db like sqlite?
loadInitInformation :: IO (S.Set Neighbor, MyNodePosition)
loadInitInformation = undefined


nodeSaveShard :: Shard -> (ShardingNode ->  IO ()) -> ShardingNode -> IO ()
nodeSaveShard aShard aLoop aShardingNode = undefined


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


sendToNetLevet :: Chan T.ManagerMiningMsgBase -> ShardingNodeRequestMsg -> IO ()
sendToNetLevet aChan aMsg = writeChan aChan $ T.ShardingNodeRequestMsg aMsg


mul :: Word64 -> Word64 -> Word64
mul x y
    | aResult > toInteger (maxBound :: Word64) = maxBound
    | otherwise = fromInteger aResult
  where
    aResult = (toInteger x * toInteger y) `div` distanceNormalizedCapture
