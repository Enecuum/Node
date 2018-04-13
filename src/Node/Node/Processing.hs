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
import              Data.IORef
import              System.Clock
import              System.Random
import              Lens.Micro
import              Lens.Micro.Mtl
import              Control.Concurrent
import              Control.Monad.Extra
import              Crypto.Error

import              Node.Node.Base
import              Node.Node.Types
import              Node.Crypto
import              Node.Data.Data
import              Node.Data.NodeTypes
import              Node.Data.NetPackage
import              Sharding.Sharding
import qualified    Sharding.Types.Node as T
import              Sharding.Space.Point
import              Sharding.Space.Distance
import              Node.Data.MakeAndSendTraceRouting
import              Node.Data.MakeTraceRouting

class Processing aNodeData aPackage where
    processing
        ::  ManagerMsg msg
        =>  Chan msg
        ->  aNodeData
        ->  PackageSignature
        ->  TraceRouting
        ->  aPackage
        ->  IO ()


instance Processing (IORef ManagerNodeData) (Responce NetLvl) where
    processing aChan aMd (PackageSignature (toNodeId -> aNodeId) _ _) _ = \case
        BroadcastListResponce aBroadcastListLogic aBroadcastList -> do
            aData <- readIORef aMd
            let aMyNodeId = aData^.myNodeId
            addRecordsToNodeListFile aMyNodeId aBroadcastListLogic
            addRecordsToNodeListFile aMyNodeId aBroadcastList
            let NodeInfoListNetLvl aList = aBroadcastList
            when (null aList) $ do
                aDeltaX <- randomIO
                aDeltaY <- randomIO
                let aMyNodePosition = MyNodePosition $ Point aDeltaX aDeltaY
                aChanOfSharding <- newChan
                makeShardingNode aMyNodeId aChanOfSharding aChan aMyNodePosition
                modifyIORef aMd (&~ do
                    myNodePosition .= Just aMyNodePosition
                    shardingChan   .= Just aChanOfSharding)

        HostAdressResponce _ -> return ()

        IAmBroadcast          aBool          -> do
            modifyIORef aMd $ nodes %~ M.adjust (isBroadcast .~ aBool) aNodeId


instance Processing (IORef ManagerNodeData) (Responce LogicLvl) where
    processing _ aMd (PackageSignature (toNodeId -> aNodeId) _ _) _ aResponse = do
        aData <- readIORef aMd
        case aResponse of
            ShardIndexResponce aShardHashList ->
                sendToShardingLvl aData $ T.ShardIndexAcceptAction aShardHashList

            ShardResponce      aShardes         -> forM_ aShardes $
                sendToShardingLvl aData . T.ShardAcceptAction

            NodePositionResponcePackage (toNodePosition -> aNodePosition) -> do
                updateFile (aData^.myNodeId) (NodeInfoListLogicLvl [(aNodeId, aNodePosition)])
                sendToShardingLvl aData $
                    T.TheNodeHaveNewCoordinates aNodeId aNodePosition
--
instance Processing (IORef ManagerNodeData) (Request LogicLvl) where
    processing _ aMd aSignature@(PackageSignature (toNodeId -> aNodeId) _ _) aTraceRouting aRequestLogicLvl = do
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
                        T.ShardLoadAction aChan aNodeId aShardHash
                    T.ShardResponse aShard <- readChan aChan
                    return aShard

            NodePositionRequestPackage ->
                aRequestToNetLvl NodePositionResponcePackage $ do
                    aChan <- newChan
                    sendToShardingLvl aData $
                        T.NodePositionAction aChan aNodeId
                    T.NodePositionResponse aMyNodePosition <- readChan aChan
                    modifyIORef aMd $ myNodePosition .~ Just aMyNodePosition
                    return aMyNodePosition

            IsAliveTheNodeRequestPackage aNodeId -> do
                let aMaybeNodeId = case aTraceRouting of
                        ToNode _ (PackageSignature (toNodeId -> aId) _ _)
                            -> Just aId
                        _   -> Nothing

                whenJust aMaybeNodeId $ \aJustNodeId -> do
                    let aNetLvlPackage = TheNodeIsAlive
                            aNodeId $ (aData^.nodes.at aNodeId) /= Nothing
                    aResponsePackageSignature <- makePackageSignature aData
                        (aNetLvlPackage, aRequestPackage)
                    let aPackage = ResponceLogicLvlPackage
                            aRequestPackage aNetLvlPackage aResponsePackageSignature
                    aTrace <- makeTraceRouting  aData aPackage
                        (ToNode aJustNodeId)
                    sendResponse (aData^.nodes.at aJustNodeId) aTrace aPackage


{-

aTraceRouting = (ToNode aNodeId (PackageSignature aMyNodeId _ _))

PackageSignature :: MyNodeId -> TimeSpec  -> Signature  -> PackageSignature
-}

{-
            IsAliveTheNodeRequestPackage    :: NodeId -> Request LogicLvl
            TheNodeIsAlive                :: NodeId -> Bool -> Responce LogicLvl
-}

{-
requestToNetLvl aData aTraceRouting aRequestPackage aConstructor aLogicRequest =
    void $ forkIO $ do
        aResultOfRequest <- aLogicRequest
        let (aNode, aTrace) = getClosedNode aTraceRouting aData
            aNetLevetPackage = aConstructor aResultOfRequest

        aResponsePackageSignature <- makePackageSignature aData
            (aNetLevetPackage, aRequestPackage)

        sendResponse aNode
            (makeNewTraceRouting aTrace aTraceRouting)
            (ResponceLogicLvlPackage aRequestPackage aNetLevetPackage aResponsePackageSignature)

-}

instance Processing (IORef ManagerNodeData) (Request NetLvl) where
    processing _ aMd aSignature aTraceRouting aRequest = do
        aData <- readIORef aMd
        let aSendNetLvlResponse = sendNetLvlResponse
                aTraceRouting aData aRequest aSignature
        case aRequest of
            IsYouBrodcast -> aSendNetLvlResponse
                (IAmBroadcast $ aData^.iAmBroadcast)

            HostAdressRequest -> aSendNetLvlResponse
                (HostAdressResponce $ aData^.hostAddress)

            BroadcastListRequest -> do
                -- TEMP Think about move aBroadcastList to operacety memory.
                NodeInfoListNetLvl   aBroadcastList      <- readRecordsFromNodeListFile $ aData^.myNodeId
                NodeInfoListLogicLvl aBroadcastListLogic <- readRecordsFromNodeListFile $ aData^.myNodeId
                let aBroadcastListResponce = BroadcastListResponce
                        (NodeInfoListLogicLvl $ take 10 aBroadcastListLogic)
                        (NodeInfoListNetLvl   $ take 10 aBroadcastList)

                aSendNetLvlResponse aBroadcastListResponce



sendToShardingLvl :: ManagerData md => md -> T.ShardingNodeAction -> IO ()
sendToShardingLvl aData aMsg = whenJust (aData^.shardingChan) $ \aChan ->
    writeChan aChan aMsg


sendNetLvlResponse
    :: ManagerData md
    =>  TraceRouting
    ->  md
    ->  Request NetLvl
    ->  PackageSignature
    ->  Responce NetLvl
    ->  IO ()

sendNetLvlResponse aTraceRouting aData aRequest aSignature aNetPackage = do
    let (aNode, aTrace) = getClosedNode aTraceRouting aData
        aRequestPackage = request aRequest aSignature

    aResponsePackageSignature <- makePackageSignature aData
        (aNetPackage, aRequestPackage)
    sendResponse aNode
        (makeNewTraceRouting aTrace aTraceRouting)
        (ResponceNetLvlPackage aRequestPackage aNetPackage aResponsePackageSignature)

-- TEMP: requestToNetLvl + sendNetLvlResponse
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

        aResponsePackageSignature <- makePackageSignature aData
            (aNetLevetPackage, aRequestPackage)

        sendResponse aNode
            (makeNewTraceRouting aTrace aTraceRouting)
            (ResponceLogicLvlPackage aRequestPackage aNetLevetPackage aResponsePackageSignature)


getClosedNode
    ::  ManagerData md
    =>  TraceRouting
    ->  md
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


sendResponse :: Maybe Node -> TraceRouting -> ResponcePackage -> IO ()
sendResponse aNode aTraceRouting aPackageResponse = whenJust aNode $
    sendToNode (makeResponse aTraceRouting aPackageResponse)


makeNewTraceRouting :: [PackageSignature] -> TraceRouting -> TraceRouting
makeNewTraceRouting aSignatures = \case
    ToDirect aPointFrom aPointTo _  -> ToDirect aPointFrom aPointTo aSignatures
    aTraceRouting                   -> aTraceRouting


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



--------------------------------------------------------------------------------
