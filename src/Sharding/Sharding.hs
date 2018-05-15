{-# LANGUAGE
        LambdaCase
    ,   MultiWayIf
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
import qualified    Data.ByteString.Lazy as L
import              Control.Concurrent.Chan
import qualified    Data.Aeson as A
import              Data.List.Extra
import              Control.Concurrent
import              Lens.Micro
import              Data.Word
import              Service.Timer
import qualified    Data.Set            as S
import qualified    Data.Map            as M
import              System.Clock
import              Lens.Micro.Mtl

import              Node.Data.GlobalLoging
import              Service.InfoMsg
import              Lens.Micro.GHC()
import              Node.Data.Key

sizeOfShardStore:: Int
sizeOfShardStore = 500

smallPeriod :: Int
smallPeriod = 10^(6::Int)

bigPeriod :: Int
bigPeriod = 10^(8::Int)

-- TODO loading of shards to sharding lvl.

numberOfLoadingShards :: ShardingNode -> Int
numberOfLoadingShards aShardingNode =
    length $ aShardingNode^.nodeIndex.shardLoadingIndex.setOfLoadingShards


numberOfNeededShards :: ShardingNode -> Int
numberOfNeededShards aShardingNode =
    length $ aShardingNode^.nodeIndex.shardNeededIndex.setOfHash


-- COMBAK: Understand what is the problem.
-- nodeIndex.shardNeededIndex.setOfHash

makeShardingNode :: T.ManagerMsg msg =>
                          MyNodeId
                          -> Chan ShardingNodeAction
                          -> Chan msg
                          -> MyNodePosition
                          -> Chan InfoMsg
                          -> IO ()
makeShardingNode aMyNodeId aChanRequest aChanOfNetLevel aMyNodePosition infoMsgChan = do
    aShardingNode <- initOfShardingNode aChanOfNetLevel aChanRequest aMyNodeId aMyNodePosition infoMsgChan
    writeLog infoMsgChan [ShardingLvlTag, InitTag] Info "Start of sharding lvl."
    void $ forkIO $ aLoop aShardingNode
  where
    aLoop :: ShardingNode -> IO ()
    aLoop aShardingNode = do
      writeLog infoMsgChan [ShardingLvlTag] Info "Sharding loop start."
      readChan aChanRequest >>= \case
        CheckOfShardLoadingList -> do
            writeLog infoMsgChan [ShardingLvlTag] Info "Check loading sharding list."
            aNow <- getTime Realtime
            let aCondition (_, _, aTime) = diffTimeSpec aNow aTime < fromNanoSecs (toInteger bigPeriod)
                aLoadingShards = aShardingNode^.nodeIndex.shardLoadingIndex.setOfLoadingShards
                (aFresh, aOld) = partition aCondition aLoadingShards
            aLoop $ flip execState aShardingNode $
                zoom nodeIndex $ do
                    shardNeededIndex.setOfHash %= S.union
                        (S.fromList $ (^._1) <$> aOld)
                    shardLoadingIndex.setOfLoadingShards .= aFresh

        ShardCheckLoading
            | numberOfLoadingShards aShardingNode < 4 &&
                numberOfNeededShards aShardingNode /= 0 -> do
                    writeLog infoMsgChan [ShardingLvlTag] Info
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
            | otherwise -> aLoop aShardingNode

        ShiftAction | shiftIsNeed aShardingNode -> do
            writeLog infoMsgChan [ShardingLvlTag] Info "Try shift."
            shiftTheShardingNode aChanOfNetLevel aLoop aShardingNode
                    | otherwise -> aLoop aShardingNode

        CheckTheNeighbors -> do
            let aNodePositions      = (^.neighborPosition) `S.map` aNeighbors
                aNeighborsPositions = findNearestNeighborPositions aMyNodePosition aNodePositions
                aNeighbors = aShardingNode^.nodeNeighbors
                aFilteredNeighbors = filter
                    (\a -> a^.neighborPosition `elem` aNeighborsPositions)
                    $ S.toList aNeighbors

            writeLog infoMsgChan [ShardingLvlTag] Info $ "Check neighbors."
                ++ " Node position: " ++ show aNodePositions
                ++ ", Node positions: " ++ show  (S.toList aNodePositions)
                ++ ", Neighbors positions: " ++ show (S.toList aNeighbors)
                ++ ", Filtered neighbors positions: " ++ show aFilteredNeighbors

            forM_ aFilteredNeighbors $ \aNeighbor ->
                sendToNetLevet aChanOfNetLevel $ IsTheNeighborAliveRequest
                    (aNeighbor^.neighborId)
                    (aNeighbor^.neighborPosition)
            aLoop aShardingNode

        TheNodeHaveNewCoordinates aNodeId aNodePosition
            | isInNodeDomain aShardingNode aNodePosition -> do
                writeLog infoMsgChan [ShardingLvlTag] Info $
                     "The node " ++ show aNodeId
                  ++ " have new posiotion: " ++ show aNodePosition
                  ++ " and save it position"
                aLoop
                  $ insertTheNeighbor aNodeId aNodePosition
                  $ deleteTheNeighbor aNodeId aShardingNode
            | otherwise -> do
                writeLog infoMsgChan [ShardingLvlTag] Info $
                     "The node " ++ show aNodeId
                  ++ " have new posiotion: " ++ show aNodePosition
                  ++ " , remove this node because it isn't my neighbor more."
                aLoop
                  $ deleteTheNeighbor aNodeId aShardingNode

        TheNodeIsDead aNodeId -> do
            writeLog infoMsgChan [ShardingLvlTag] Info $
               "This node " ++ show aNodeId ++ " is dead."
            aLoop $ deleteTheNeighbor aNodeId aShardingNode

        ShardRequestAction  aShardHash aChan -> do
            writeLog infoMsgChan [ShardingLvlTag] Info $
                "Somebody ask shard " ++ show aShardHash ++ "."
            aMaybeShard <- loadShard aShardHash
            case aMaybeShard of
                Just aShard -> do
                    writeLog infoMsgChan [ShardingLvlTag] Info $
                        "This shard " ++ show aShardHash ++ " store hire."
                    writeChan aChan aShard
                    aLoop aShardingNode
                Nothing -> do
                    writeLog infoMsgChan [ShardingLvlTag] Info $
                        "This shard " ++ show aShardHash
                     ++ " doesn't store hire. Request neighbors of this shard."
                    sendToNetLevet aChanOfNetLevel $ ShardListRequest [aShardHash]
                    aTime <- getTime Realtime
                    aLoop $ aShardingNode & nodeIndexOfReques %~
                        M.insert aShardHash (aTime, aChan)

        ShardIndexAcceptAction aShardHashs -> do
            writeLog infoMsgChan [ShardingLvlTag] Info $
                "Accept shard index list " ++ show (S.fromList aShardHashs)
            aLoop $ addShardingIndex (S.fromList aShardHashs) aShardingNode

        ShardIndexCreateAction aChan aNodeId aRadiusOfCapture -> do
            writeLog infoMsgChan [ShardingLvlTag] Info $
                "This node " ++ show aNodeId
             ++ "with this radius " ++ show aRadiusOfCapture
             ++ " ask shard index."
            createShardingIndex aChan aShardingNode aNodeId aRadiusOfCapture
            aLoop aShardingNode

        ShardAcceptAction aShard
            | Just (_, aChan) <- aShardingNode^.nodeIndexOfReques.at (shardToHash aShard) -> do
                writeLog infoMsgChan [ShardingLvlTag] Info $
                    "This shard " ++ show aShard ++ " accepted and sended to request channel."
                writeChan aChan aShard
                aLoop $ aShardingNode & nodeIndexOfReques %~ M.delete (shardToHash aShard)

            | checkShardIsInRadiusOfCaptureShardingNode aShardingNode (shardToHash aShard) -> do
                writeLog infoMsgChan [ShardingLvlTag] Info $
                    "This shard " ++ show aShard ++ " accepted and saved localy."
                nodeSaveShard aShard aLoop aShardingNode
            | otherwise -> aLoop aShardingNode

        NewShardInNetAction aShard
            | checkShardIsInRadiusOfCaptureShardingNode aShardingNode (shardToHash aShard) -> do
                writeLog infoMsgChan [ShardingLvlTag] Info $
                    "New shard " ++ show aShard ++ " in net and save localy."
                nodeSaveShard aShard aLoop aShardingNode
            | otherwise -> aLoop aShardingNode

        ShardLoadAction aChan aNodeId aHashList -> do
            writeLog infoMsgChan [ShardingLvlTag] Info $
                "Node  " ++ show aNodeId ++ " want this shards "
              ++ show aHashList ++ "."
            sendShardsToNode aHashList aChan
            aLoop aShardingNode

        NodePositionAction aChan aNodeId -> do
            let aMyPosition = aShardingNode^.nodePosition
            writeLog infoMsgChan [ShardingLvlTag] Info $
                "This node  " ++ show aNodeId ++ " ask my position "
              ++ show aMyPosition ++ "."
            writeChan aChan $ NodePositionResponse aMyPosition
            aLoop aShardingNode

        NeighborListAcceptAction aNeighborList -> do
            writeLog infoMsgChan [ShardingLvlTag] Info $
                "Accept neighbor list  " ++ show aNeighborList ++ "."
            aLoop $ flip execState aShardingNode $
                forM_ aNeighborList $ \(aNodeId, aNodePosition) -> do
                    aNewShardingNode <- get
                    when (isInNodeDomain aNewShardingNode aNodePosition) $ do
                        modify (deleteTheNeighbor aNodeId)
                        modify (insertTheNeighbor aNodeId aNodePosition)

        CleanShardsAction -> do
            writeLog infoMsgChan [GCTag] Info "Clean shards action."
            let aNodeDomain = findShardingNodeDomain aShardingNode
                (aNewShardIndex, aOldShardHashList) = cleanShardIndex
                    (aShardingNode^.nodePosition)
                    aNodeDomain
                    sizeOfShardStore
                    (aShardingNode^.nodeIndex)
            forM_ aOldShardHashList removeShard
            aLoop $ aShardingNode & nodeIndex .~ aNewShardIndex

        CleanNeededIndex -> do
            writeLog infoMsgChan [GCTag] Info "Clean needed index action."
            let aNodeDomain = findShardingNodeDomain aShardingNode
                aCondition aHash = distanceTo
                    (aShardingNode^.nodePosition) aHash < aNodeDomain
                aFilter = S.filter aCondition
            aLoop $ aShardingNode & nodeIndex.shardNeededIndex.setOfHash %~ aFilter

        CleanRequestIndex -> do
            writeLog infoMsgChan [GCTag] Info "Clean request index action."
            aTime <- getTime Realtime
            aLoop $ aShardingNode & nodeIndexOfReques %~
                M.filter (\a -> diffTimeSpec (a^._1) aTime < fromNanoSecs (toInteger bigPeriod))

        aError                 -> do
            writeLog infoMsgChan [ShardingLvlTag] Warning $
                "Sharding.Sharding.makeShardingNode " ++ show aError
            aLoop aShardingNode

--------------------------------------------------------------------------------
--                              INTERNAL                                      --
--------------------------------------------------------------------------------
initOfShardingNode :: T.ManagerMsg msg =>
                            Chan msg
                            -> Chan ShardingNodeAction
                            -> MyNodeId
                            -> MyNodePosition
                            -> Chan InfoMsg
                            -> IO ShardingNode
initOfShardingNode aChanOfNetLevel aChanRequest aMyNodeId aMyNodePosition infoMsgChan = do
    writeLog infoMsgChan [ShardingLvlTag, InitTag] Info "Init sharding node"
    sendToNetLevet aChanOfNetLevel $ IamAwakeRequst aMyNodeId aMyNodePosition
    sendToNetLevet aChanOfNetLevel NeighborListRequest

    aMyShardsIndex <- loadMyShardIndex

    metronome bigPeriod $ writeChan aChanRequest CleanShardsAction
    metronome bigPeriod $ writeChan aChanRequest CleanNeededIndex
    metronome bigPeriod $ writeChan aChanRequest CleanRequestIndex
    metronome smallPeriod $ writeChan aChanRequest CheckOfShardLoadingList
    void $ forkIO $ do
        threadDelay $ smallPeriod * 20
        metronomeLinear smallPeriod bigPeriod $ do
            writeChan aChanRequest CheckTheNeighbors
            threadDelay smallPeriod
            writeChan aChanRequest ShiftAction

    enc <- L.readFile "configs/config.json"
    case A.decode enc of
        Nothing -> error "config not is valid"
        Just aEnc
            |  T.sharding aEnc == "off" -> return $ makeEmptyShardingNode S.empty aMyNodeId aMyNodePosition aMyShardsIndex infoMsgChan maxBound
            | otherwise              ->  return $ makeEmptyShardingNode S.empty aMyNodeId aMyNodePosition aMyShardsIndex infoMsgChan 1

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
    writeLog (aShardingNode^.nodeInfoMsgChan) [ShardingLvlTag] Info $
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


createShardingIndex :: Chan ShardingNodeResponse -> ShardingNode -> NodeId -> Word64 ->  IO ()
createShardingIndex aChanOfNetLevel aShardingNode aNodeId aRadiusOfCapture = do
    let aMaybeNeighbor = S.toList $ S.filter (\n -> n^.neighborId == aNodeId) $
            aShardingNode^.nodeNeighbors
    case aMaybeNeighbor of
        [Neighbor aNeighborPosition _] -> do
            let resultsShardingHash = S.filter
                    (checkShardIsInRadiusOfCapture aNeighborPosition aRadiusOfCapture)
                    (indexToSet $ aShardingNode^.nodeIndex)
            writeChan aChanOfNetLevel (ShardIndexResponse $ S.toList resultsShardingHash)
        _  -> writeChan aChanOfNetLevel (ShardIndexResponse [])


checkShardIsInRadiusOfCaptureShardingNode :: ShardingNode -> ShardHash -> Bool
checkShardIsInRadiusOfCaptureShardingNode aShardNode =
    checkShardIsInRadiusOfCapture
        (toNodePosition $ aShardNode^.nodePosition)
        (mul    (findShardingNodeDomain aShardNode)
                (distanceNormalizedCapture + aShardNode^.nodeDistance))


sendShardsToNode ::
        ShardHash
    ->  Chan ShardingNodeResponse
    ->  IO ()
sendShardsToNode aHashList aChanOfNetLevel = do
    aShards <- loadShards [aHashList]
    writeChan aChanOfNetLevel $ ShardResponse aShards

--------------------------------------------------------------------------------
findNodeDomain :: MyNodePosition -> S.Set NodePosition -> Distance Point
findNodeDomain aMyPosition aPositions = if
    | length aNearestPoints < 4 -> maxBound
    | otherwise                 ->
        maximum $ distanceTo aMyPosition <$> aNearestPoints
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
