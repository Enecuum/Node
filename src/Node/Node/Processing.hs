{-# LANGUAGE
    OverloadedStrings,
    MultiWayIf,
    LambdaCase,
    MultiParamTypeClasses,
    ViewPatterns,
    StandaloneDeriving,
    TypeSynonymInstances,
    FlexibleContexts,
    TypeFamilies,
    FlexibleInstances
     #-}
module Node.Node.Processing where

import qualified    Data.Map                        as M
import              Data.List.Extra
import              System.Clock
import              Data.IORef
import              Lens.Micro
import              Control.Concurrent
import              Control.Monad.Extra
import              Node.Node.Base
import              Node.Node.Types
import              Node.Crypto
import              Node.Data.NodeTypes
import              Node.Data.NetPackage
import qualified    Sharding.Types.Node as T
import              Sharding.Space.Point
import              Sharding.Space.Distance



class Processing aNodeData aPackage where
    processing ::
            aNodeData
        ->  PackageSignature
        ->  TraceRouting
        ->  aPackage
        ->  IO ()


instance Processing (IORef ManagerNodeData) ResponceNetLvl where
    processing aMd (PackageSignature (toNodeId -> aNodeId) _ _) _ = \case
        BroadcastListResponce aBroadcastList -> do
            aData <- readIORef aMd
            forM_ aBroadcastList $ \(aNodeId, aIp, aPort) -> do
                addRecordToNodeListFile (aData^.myNodeId) aNodeId aIp aPort

        HostAdressResponce    aHostAdress    -> return ()

        IAmBroadcast          aBool          -> do
            modifyIORef aMd $ nodes %~ M.adjust (isBroadcast .~ aBool) aNodeId


instance Processing (IORef ManagerNodeData) ResponceLogicLvl where
    processing aMd (PackageSignature (toNodeId -> aNodeId) _ _) _ aResponse = do
        aData <- readIORef aMd
        case aResponse of
            ShardIndexResponce aShardHashList ->
                sendToShardingLvl aData $ T.ShardIndexAcceptAction aShardHashList

            ShardResponce      aShard         ->
                sendToShardingLvl aData $ T.ShardAcceptAction aShard

--
instance Processing (IORef ManagerNodeData) RequestLogicLvl where
    processing aMd aSignature@(PackageSignature (toNodeId -> aNodeId) _ _) aTraceRouting aRequestLogicLvl = do
        aData <- readIORef aMd
        let aRequestPackage = RequestLogicLvlPackage aRequestLogicLvl aSignature

            aRequestToNetLvl :: (a -> ResponceLogicLvl) -> IO a -> IO ()
            aRequestToNetLvl = requestToNetLvl aData aTraceRouting aRequestPackage

        case aRequestLogicLvl of
            ShardIndexRequestPackage _ aDistance ->
                aRequestToNetLvl ShardIndexResponce $ do
                    aChan <- newChan
                    sendToShardingLvl aData $
                        T.ShardIndexCreateAction aChan aNodeId aDistance
                    T.ShardIndexResponse aShardIndex <- readChan aChan
                    return aShardIndex

            ShardRequestPackage aShardHash -> do
                aRequestToNetLvl ShardResponce $ do
                    aChan <- newChan
                    sendToShardingLvl aData $
                        T.ShardListCreateAction aChan aNodeId aShardHash
                    T.ShardResponse aShard <- readChan aChan
                    return aShard

            NodePositionRequestPackage ->
                aRequestToNetLvl NodePositionResponcePackage $ do
                    aChan <- newChan
                    sendToShardingLvl aData $
                        T.NodePositionAction aChan aNodeId
                    T.NodePositionResponse aMyNodePosition <- readChan aChan
                    return aMyNodePosition



instance Processing (IORef ManagerNodeData) RequestNetLvl where
    processing aMd aSignature@(PackageSignature (toNodeId -> aNodeId) _ _) aTraceRouting aRequest = do
        aData <- readIORef aMd
        let aSendNetLvlResponse = sendNetLvlResponse
                aTraceRouting aData aRequest aSignature
        case aRequest of
            IsYouBrodcast -> aSendNetLvlResponse
                (IAmBroadcast $ aData^.iAmBroadcast)

            HostAdressRequest -> aSendNetLvlResponse
                (HostAdressResponce $ aData^.hostAddress)

            BroadcastListRequest -> do
                -- TODO think about aBroadcastList
                aBroadcastList <- readRecordFromNodeListFile $ aData^.myNodeId
                aSendNetLvlResponse (BroadcastListResponce $ take 10 aBroadcastList)


-- TODO
sendToShardingLvl aData aMsg = whenJust (aData^.shardingChan) $ \aChan ->
    writeChan aChan aMsg


-- TODO  requestToNetLvl + sendNetLvlResponse
requestToNetLvl ::
        ManagerNodeData
    ->  TraceRouting
    ->  RequestPackage
    -> (a -> ResponceLogicLvl)
    ->  IO a
    ->  IO ()
requestToNetLvl aData aTraceRouting aRequestPackage aConstructor aLogicRequest =
    void $ forkIO $ do
        aResultOfRequest <- aLogicRequest
        let (aNode, aTrace) = getClosedNode aTraceRouting aData
            aNetLevetPackage = aConstructor aResultOfRequest

        aResponsePackageSignature <- makePackageSignature aData aNetLevetPackage
        sendResponse aNode
            (makeNewTraceRouting aTrace aTraceRouting)
            (ResponceLogicLvlPackage aRequestPackage aNetLevetPackage aResponsePackageSignature)


sendNetLvlResponse aTraceRouting aData aRequest aSignature aNetPackage = do
    let (aNode, aTrace) = getClosedNode aTraceRouting aData
        aRequestPackage = RequestNetLvlPackage aRequest aSignature

    aResponsePackageSignature <- makePackageSignature aData aNetPackage
    sendResponse aNode
        (makeNewTraceRouting aTrace aTraceRouting)
        (ResponceNetLvlPackage aRequestPackage aNetPackage aResponsePackageSignature)
--
getClosedNodeByDirect :: ManagerNodeData -> Point -> Maybe Node
getClosedNodeByDirect aData aPoint =
    case closedToPointNeighbor aData aPoint of
        aNode:_ | not $ amIClose aData aNode (fromPoint aPoint :: PointTo)
                -> Just aNode
        _       -> Nothing


getClosedNode :: TraceRouting -> ManagerNodeData -> (Maybe Node, [PackageSignature])
getClosedNode aTraceRouting aData = case aTraceRouting of
    ToDirect aPointFrom aPointTo aTrace
        | Just aNextNodeId <- lookupNextNode aTrace aData -> do
            let aNewTrace = traceDrop aNextNodeId aTrace
            (aNextNodeId `M.lookup` (aData^.nodes), aNewTrace)
        | otherwise -> (getClosedNodeByDirect aData (toPoint aPointTo), aTrace)
    ToNode _ (PackageSignature (toNodeId -> aNodeId) _ _) ->
        (aNodeId `M.lookup` (aData^.nodes), [])

  where
    cleanTrace aTrace = filter aPredicat $ dropWhile aPredicat aTrace
      where
        aPredicat (PackageSignature aNodeId _ _) = aData^.myNodeId /= aNodeId

    lookupNextNode aTrace aData = if
        | x:_ <- aIntersect aTrace -> Just x
        | otherwise                -> Nothing
      where
        aIntersect aTrace = (signatureToNodeId <$> aTrace) `intersect` aNeighborList
        aNeighborList     = M.keys (aData^.nodes)


makePackageSignature aData aResponse = do
    aTime <- getTime Realtime
    let aNodeId = aData^.myNodeId
    aResponceSignature <- signEncodeble
        (aData^.privateKey) (aNodeId, aTime, aResponse)
    return $ PackageSignature aNodeId aTime aResponceSignature

sendResponse aNode aNewTraceRouting aPackageResponse = whenJust aNode $
    sendToNode (makeResponse aNewTraceRouting aPackageResponse)


makeNewTraceRouting :: [PackageSignature] -> TraceRouting -> TraceRouting
makeNewTraceRouting aSignatures = \case
    ToDirect aPointFrom aPointTo _  -> ToDirect aPointFrom aPointTo aSignatures
    aTraceRouting                   -> aTraceRouting


closedToPointNeighbor aData aPointTo = sortOn
    (\n -> distanceTo n aPointTo)
        $ M.elems $ aData^.nodes
--
amIClose aData aNode aPointTo = if
    | Just aPosition <- aData^.myNodePosition,
        distanceTo aPosition aPointTo < distanceTo aNode aPointTo -> True
    | otherwise -> False


traceDrop aNextNodeId = dropWhile
    (\(PackageSignature (toNodeId -> aId) _ _) -> aId /= aNextNodeId)


makeResponse aTraceRouting aResponse = makeCipheredPackage
    (PackageTraceRoutingResponce aTraceRouting aResponse)


signatureToNodeId :: PackageSignature -> NodeId
signatureToNodeId (PackageSignature (toNodeId -> aNodeId) _ _) = aNodeId

instance DistanceTo Node Point where
    distanceTo aNode aPoint = if
        | Just aPosition <- aNode^.nodePosition ->
            distanceTo aPosition  (NodePosition aPoint)
        | otherwise                             -> maxBound

instance DistanceTo Node PointTo where
    distanceTo aNode aPoint = distanceTo aNode (toPoint aPoint)

--------------------------------------------------------------------------------
