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
import              Data.Serialize
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
import              Node.Data.Data
import              Crypto.Error

class Processing aNodeData aPackage where
    processing
        ::  aNodeData
        ->  PackageSignature
        ->  TraceRouting
        ->  aPackage
        ->  IO ()


instance Processing (IORef ManagerNodeData) (Responce NetLvl) where
    processing aMd (PackageSignature (toNodeId -> aNodeId) _ _) _ = \case
        BroadcastListResponce aBroadcastListLogic aBroadcastList -> do
            aData <- readIORef aMd

            addRecordsToNodeListFile (aData^.myNodeId) aBroadcastListLogic
            addRecordsToNodeListFile (aData^.myNodeId) aBroadcastList

        HostAdressResponce _ -> return ()

        IAmBroadcast          aBool          -> do
            modifyIORef aMd $ nodes %~ M.adjust (isBroadcast .~ aBool) aNodeId


instance Processing (IORef ManagerNodeData) (Responce LogicLvl) where
    processing aMd (PackageSignature (toNodeId -> aNodeId) _ _) _ aResponse = do
        aData <- readIORef aMd
        case aResponse of
            ShardIndexResponce aShardHashList ->
                sendToShardingLvl aData $ T.ShardIndexAcceptAction aShardHashList

            ShardResponce      aShard         ->
                sendToShardingLvl aData $ T.ShardAcceptAction aShard

            NodePositionResponcePackage (toNodePosition -> aNodePosition) -> do
                updateFile (aData^.myNodeId) (NodeInfoListLogicLvl [(aNodeId, aNodePosition)])
                sendToShardingLvl aData $
                    T.TheNodeHaveNewCoordinates aNodeId aNodePosition
--
instance Processing (IORef ManagerNodeData) (Request LogicLvl) where
    processing aMd aSignature@(PackageSignature (toNodeId -> aNodeId) _ _) aTraceRouting aRequestLogicLvl = do
        aData <- readIORef aMd
        let aRequestPackage = RequestLogicLvlPackage aRequestLogicLvl aSignature

            aRequestToNetLvl :: (a -> Responce LogicLvl) -> IO a -> IO ()
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



instance Processing (IORef ManagerNodeData) (Request NetLvl) where
    processing aMd aSignature aTraceRouting aRequest = do
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
                NodeInfoListNetLvl   aBroadcastList      <- readRecordsFromNodeListFile $ aData^.myNodeId
                NodeInfoListLogicLvl aBroadcastListLogic <- readRecordsFromNodeListFile $ aData^.myNodeId
                let aBroadcastListResponce = BroadcastListResponce
                        (NodeInfoListLogicLvl $ take 10 aBroadcastListLogic)
                        (NodeInfoListNetLvl   $ take 10 aBroadcastList)

                aSendNetLvlResponse aBroadcastListResponce


-- TODO
sendToShardingLvl :: ManagerNodeData -> T.ShardingNodeAction -> IO ()
sendToShardingLvl aData aMsg = whenJust (aData^.shardingChan) $ \aChan ->
    writeChan aChan aMsg


-- TODO  requestToNetLvl + sendNetLvlResponse
requestToNetLvl
    ::  ManagerNodeData
    ->  TraceRouting
    ->  RequestPackage
    -> (a -> Responce LogicLvl)
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

sendNetLvlResponse
    ::  TraceRouting
    ->  ManagerNodeData
    ->  Request NetLvl
    ->  PackageSignature
    ->  Responce NetLvl
    ->  IO ()

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


getClosedNode
    ::  TraceRouting
    ->  ManagerNodeData
    ->  (Maybe Node, [PackageSignature])
getClosedNode aTraceRouting aData = case aTraceRouting of
    ToDirect _ aPointTo aTrace
        | Just aNextNodeId <- lookupNextNode aTrace -> do
            let aNewTrace = traceDrop aNextNodeId aTrace
            (aNextNodeId `M.lookup` (aData^.nodes), aNewTrace)
        | otherwise -> (getClosedNodeByDirect aData (toPoint aPointTo), cleanTrace aTrace)
    ToNode _ (PackageSignature (toNodeId -> aNodeId) _ _) ->
        (aNodeId `M.lookup` (aData^.nodes), [])

  where
    cleanTrace aTrace = filter aPredicat $ dropWhile aPredicat aTrace
      where
        aPredicat (PackageSignature aNodeId _ _) = aData^.myNodeId /= aNodeId

    lookupNextNode aTrace = if
        | x:_ <- aIntersect -> Just x
        | otherwise         -> Nothing
      where
        aIntersect      = (signatureToNodeId <$> aTrace) `intersect` aNeighborList
        aNeighborList   = M.keys (aData^.nodes)


makePackageSignature
    ::  Serialize aPackage
    =>  ManagerNodeData
    ->  aPackage
    ->  IO PackageSignature
makePackageSignature aData aResponse = do
    aTime <- getTime Realtime
    let aNodeId = aData^.myNodeId
    aResponceSignature <- signEncodeble
        (aData^.privateKey)
        (aNodeId, aTime, aResponse)
    return $ PackageSignature aNodeId aTime aResponceSignature


sendResponse :: Maybe Node -> TraceRouting -> ResponcePackage -> IO ()
sendResponse aNode aTraceRouting aPackageResponse = whenJust aNode $
    sendToNode (makeResponse aTraceRouting aPackageResponse)


makeNewTraceRouting :: [PackageSignature] -> TraceRouting -> TraceRouting
makeNewTraceRouting aSignatures = \case
    ToDirect aPointFrom aPointTo _  -> ToDirect aPointFrom aPointTo aSignatures
    aTraceRouting                   -> aTraceRouting


closedToPointNeighbor
    ::  NodeBaseDataClass s
    =>  DistanceTo Node b
    =>  s
    ->  b
    ->  [Node]
closedToPointNeighbor aData aPointTo = sortOn
    (\n -> distanceTo n aPointTo) $ M.elems $ aData^.nodes
--
amIClose
    ::  DistanceTo MyNodePosition aPointB
    =>  DistanceTo aPointA aPointB
    =>  ManagerNodeData
    ->  aPointA
    ->  aPointB
    ->  Bool
amIClose aData aNode aPointTo = if
    | Just aPosition <- aData^.myNodePosition,
        distanceTo aPosition aPointTo < distanceTo aNode aPointTo -> True
    | otherwise -> False

traceDrop :: NodeId -> [PackageSignature] -> [PackageSignature]
traceDrop aNextNodeId = dropWhile
    (\(PackageSignature (toNodeId -> aId) _ _) -> aId /= aNextNodeId)


makeResponse
    ::  TraceRouting
    ->  ResponcePackage
    ->  StringKey
    ->  CryptoFailable Package
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
