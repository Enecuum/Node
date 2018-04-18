{-# LANGUAGE
        LambdaCase
    ,   MultiWayIf
    ,   ViewPatterns
    ,   FlexibleContexts
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

import              Control.Monad.State.Lazy
import qualified    Node.Node.Types     as T
import              Control.Concurrent.Chan
import              Data.List.Extra
import              Control.Concurrent
import              Lens.Micro
import              Data.Word
import              Service.Timer
import qualified    Data.Set            as S
import qualified    Data.Map            as M
import              System.Clock
import              Lens.Micro.Mtl

import              Node.Data.NodeTypes
import              Node.Data.GlobalLoging
import              Service.InfoMsg
import              Lens.Micro.GHC

sizeOfShardStore:: Int
sizeOfShardStore = 500

-- TODO loading of shards to sharding lvl.

numberOfLoadingShards aShardingNode =
    length $ aShardingNode^.nodeIndex.shardLoadingIndex.setOfLoadingShards

numberOfNeededShards aShardingNode =
    length $ aShardingNode^.nodeIndex.shardNeededIndex.setOfHash


-- COMBAK: Understand what is the problem.
-- nodeIndex.shardNeededIndex.setOfHash

--makeShardingNode :: MyNodeId -> Point -> IO ()
makeShardingNode aMyNodeId aChanRequest aChanOfNetLevel aMyNodePosition infoMsgChan = do
    aShardingNode <- initOfShardingNode aChanOfNetLevel aChanRequest aMyNodeId aMyNodePosition
    writeLog infoMsgChan [ShardingLvlTag] Info $  "Start of sharding lvl."
    void $ forkIO $ aLoop aShardingNode
  where
    aLoop :: ShardingNode -> IO ()
    aLoop aShardingNode = readChan aChanRequest >>= \case
        CheckOfShardLoadingList -> do
            writeLog infoMsgChan [ShardingLvlTag] Info $ "Check loading sharding list."
            aNow <- getTime Realtime
            let aCondition (_, _, aTime) = diffTimeSpec aNow aTime < fromNanoSecs (10^8)
                aLoadingShards = aShardingNode^.nodeIndex.shardLoadingIndex.setOfLoadingShards
                (aFresh, aOld) = partition aCondition aLoadingShards
            aLoop $ flip execState aShardingNode $ do
                zoom nodeIndex $ do
                    shardNeededIndex.setOfHash %= S.union
                        (S.fromList $ (^._1) <$> aOld)
                    shardLoadingIndex.setOfLoadingShards .= aFresh

        ShardCheckLoading
            | numberOfLoadingShards aShardingNode < 4 &&
                numberOfNeededShards aShardingNode /= 0 -> do
                    writeLog infoMsgChan [ShardingLvlTag] Info $
                        "Check loading shard. Loading shards < 4 and needed shards > 0."
                    let aNumberOfLoading = 4 - numberOfLoadingShards aShardingNode
                        aShardHashes = S.take aNumberOfLoading $
                            aShardingNode^.nodeIndex.shardNeededIndex.setOfHash
                    writeLog infoMsgChan [ShardingLvlTag] Info $
                        "Request shards by hash: " ++ show (S.toList aShardHashes)
                    sendToNetLevet aChanOfNetLevel $
                        ShardListRequest (S.toList aShardHashes)
                    aLoop $ aShardingNode &
                        nodeIndex.shardNeededIndex.setOfHash %~ S.drop aNumberOfLoading

        ShiftAction | shiftIsNeed aShardingNode -> do
            writeLog infoMsgChan [ShardingLvlTag] Info $ "Try shift."
            shiftTheShardingNode aChanOfNetLevel aLoop aShardingNode infoMsgChan

        CheckTheNeighbors -> do
            let aMyNodePosition     = aShardingNode^.nodePosition
                aNodePositions      = (^.neighborPosition) `S.map` aNeighbors
                aNeighborsPositions = findNearestNeighborPositions aMyNodePosition aNodePositions
                aNeighbors = aShardingNode^.nodeNeighbors
                aFilteredNeighbors = filter
                    (\a -> a^.neighborPosition `elem` aNeighborsPositions)
                    $ S.toList aNeighbors

            writeLog infoMsgChan [ShardingLvlTag] Info $ "Check neighbors."
                ++ " Node position: " ++ show aNodePositions
                ++ ", Node positions: " ++ show  (S.toList aNodePositions)
                ++ ", Neighbors positions: " ++ show (S.toList aNeighbors)

            forM_ aFilteredNeighbors $ \aNeighbor -> do
                sendToNetLevet aChanOfNetLevel $ IsTheNeighborAliveRequest
                    (aNeighbor^.neighborId)
                    (aNeighbor^.neighborPosition)
            aLoop aShardingNode

        TheNodeHaveNewCoordinates aNodeId aNodePosition
            | isInNodeDomain aShardingNode aNodePosition -> aLoop
                $ insertTheNeighbor aNodeId aNodePosition
                $ deleteTheNeighbor aNodeId aShardingNode
            | otherwise -> aLoop
                $ deleteTheNeighbor aNodeId aShardingNode

        TheNodeIsDead aNodeId -> aLoop
            $ deleteTheNeighbor aNodeId aShardingNode

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

        ShardIndexCreateAction aChan aNodeId aRadiusOfCapture -> do
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

        ShardLoadAction aChan aNodeId aHashList -> do
            sendShardsToNode aShardingNode aNodeId aHashList aChan
            aLoop aShardingNode

        NodePositionAction aChan aNodeId -> do
            writeChan aChan $ NodePositionResponse (aShardingNode^.nodePosition)
            aLoop aShardingNode

        NeighborListAcceptAction aNeighborList -> aLoop $
            flip execState aShardingNode $ do
                forM_ aNeighborList $ \(aNodeId, aNodePosition) -> do
                    aNewShardingNode <- get
                    when (isInNodeDomain aNewShardingNode aNodePosition) $ do
                        modify (deleteTheNeighbor aNodeId)
                        modify (insertTheNeighbor aNodeId aNodePosition)

        CleanShardsAction -> do
            let aNodeDomain = findShardingNodeDomain aShardingNode
                (aNewShardIndex, aOldShardHashList) = cleanShardIndex
                    (aShardingNode^.nodePosition)
                    aNodeDomain
                    sizeOfShardStore
                    (aShardingNode^.nodeIndex)
            forM_ aOldShardHashList removeShard
            aLoop $ aShardingNode & nodeIndex .~ aNewShardIndex

        CleanNeededIndex -> do
            let aNodeDomain = findShardingNodeDomain aShardingNode
                aCondition aHash = distanceTo
                    (aShardingNode^.nodePosition) aHash < aNodeDomain
                aFilter = S.filter aCondition
            aLoop $ aShardingNode & nodeIndex.shardNeededIndex.setOfHash %~ aFilter

        CleanRequestIndex -> do
            aTime <- getTime Realtime
            aLoop $ aShardingNode & nodeIndexOfReques %~
                M.filter (\a -> diffTimeSpec (a^._1) aTime < fromNanoSecs (10^8))

        a -> error $ "Sharding.Sharding.makeShardingNode"


--------------------------------------------------------------------------------
--                              INTERNAL                                      --
--------------------------------------------------------------------------------
initOfShardingNode aChanOfNetLevel aChanRequest aMyNodeId aMyNodePosition = do
    sendToNetLevet aChanOfNetLevel $ IamAwakeRequst aMyNodeId aMyNodePosition
    sendToNetLevet aChanOfNetLevel $ NeighborListRequest

    aMyShardsIndex <- loadMyShardIndex

    metronome (10^8) $ do
        writeChan aChanRequest CleanShardsAction

    metronome (10^8) $ do
        writeChan aChanRequest CleanNeededIndex

    metronome (10^8) $ do
        writeChan aChanRequest CleanRequestIndex

    metronome (10^6) $ do
        writeChan aChanRequest CheckOfShardLoadingList

    metronome (10^8) $ do
        writeChan aChanRequest CheckTheNeighbors
        threadDelay (10^6)
        writeChan aChanRequest ShiftAction

    return $ makeEmptyShardingNode S.empty aMyNodeId aMyNodePosition aMyShardsIndex


shiftTheShardingNode :: T.ManagerMsg msg =>
        Chan msg
    -> (ShardingNode ->  IO ())
    ->  ShardingNode
    ->  Chan InfoMsg
    ->  IO ()
shiftTheShardingNode aChanOfNetLevel aLoop aShardingNode infoMsgChan = do
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
    writeLog infoMsgChan [ShardingLvlTag] Info $
          "Make shift action. "
        ++ "Neighbor positions: " ++ show (S.toList aNeighborPositions)
        ++ ". My position: " ++ show aMyNodePosition
        ++ ". Nearest positions: " ++ show (S.toList aNearestPositions)
        ++ ". New position: " ++ show aNewPosition

    sendToNetLevet aChanOfNetLevel $ NewPosiotionMsg aNewPosition
    sendToNetLevet aChanOfNetLevel $ ShardIndexRequest
        (findShardingNodeDomain aShardingNode)
        (S.toList aNearestPositions)
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
    aShards <- loadShards [aHashList]
    writeChan aChanOfNetLevel $ ShardResponse aShards

--------------------------------------------------------------------------------
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
