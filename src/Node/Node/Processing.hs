{-# LANGUAGE
    OverloadedStrings,
    MultiWayIf,
    LambdaCase,
    MultiParamTypeClasses,
    ViewPatterns,
    TypeSynonymInstances,
    FlexibleContexts,
    TypeFamilies,
    FlexibleInstances
  #-}
module Node.Node.Processing where

import qualified    Data.Map                        as M
import              Data.List.Extra
import              Data.IORef
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
import              Node.Data.MakeAndSendTraceRouting
import              Node.Data.MakeTraceRouting
import              Node.Data.GlobalLoging
import              Service.InfoMsg
import              Data.Maybe
import              PoA.Types


class Processing aNodeData aPackage where
    processing
        ::  ManagerMsg msg
        =>  Chan msg
        ->  aNodeData
        ->  PackageSignature
        ->  TraceRouting
        ->  aPackage
        ->  IO ()


-- | Обработка "ответа" для сетевого уровня.
instance Processing (IORef ManagerNodeData) (Responce NetLvl) where
    processing aChan aMd (PackageSignature (toNodeId -> aNodeId) _ _) _ = \case
        BroadcastListResponce aBroadcastListLogic aBroadcastList -> do
            aData <- readIORef aMd
            writeLog (aData^.infoMsgChan) [NetLvlTag] Info
                "Accepted lists of broadcasts and points of node."
            let aMyNodeId = aData^.myNodeId

            -- добавление соответсвующих записей в списки коннектов и координат.
            writeLog (aData^.infoMsgChan) [NetLvlTag] Info
                "Add connects to list."
            writeLog (aData^.infoMsgChan) [NetLvlTag] Info
                "Add node coordinate to coordinate list."

            addRecordsToNodeListFile aMyNodeId aBroadcastListLogic
            addRecordsToNodeListFile aMyNodeId aBroadcastList
            let NodeInfoListNetLvl aList = aBroadcastList
            when (null aList) $ do
                aDeltaX <- randomIO
                aDeltaY <- randomIO
                let aMyNodePosition = MyNodePosition $ Point aDeltaX aDeltaY
                aChanOfSharding <- newChan
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info
                    "Select new random coordinate because I am first node in net."
                makeShardingNode aMyNodeId aChanOfSharding aChan aMyNodePosition (aData^.infoMsgChan)
                modifyIORef aMd (&~ do
                    myNodePosition .= Just aMyNodePosition
                    shardingChan   .= Just aChanOfSharding)

        HostAdressResponce _ -> return ()

        -- нода сказала, что она бродкаст меняем её статус в нашей памяти.
        IAmBroadcast          aBool          -> do
            aData <- readIORef aMd
            writeLog (aData^.infoMsgChan) [NetLvlTag] Info $"Node " ++ show aNodeId
                ++ " talk that it is broadcast. Changing the node status."
            modifyIORef aMd $ nodes %~ M.adjust (isBroadcast .~ aBool) aNodeId


-- обработка ответов для логического уровня.
instance Processing (IORef ManagerNodeData) (Responce LogicLvl) where
    processing _ aMd (PackageSignature (toNodeId -> aNodeId) _ _) _ aResponse = do
        aData <- readIORef aMd
        case aResponse of
            -- Accepted the index of shards from the neighbor node.
            ShardIndexResponce aShardHashList -> do
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
                    "Send from netLvl to sharding lvl the shards hashes list. "
                    ++ show aShardHashList
                sendToShardingLvl aData $ T.ShardIndexAcceptAction aShardHashList

            -- Accepted the shard from the neighbor node.
            ShardResponce      aShardes         -> do
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
                    "Send from netLvl toSharding lvl the shards acepted from neighbor "
                    ++ "the list of shards is:" ++ show aShardes
                forM_ aShardes $ sendToShardingLvl aData . T.ShardAcceptAction

            -- Accepted the node position of a neighbor node.
            NodePositionResponcePackage (toNodePosition -> aNodePosition) -> do
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
                    "Accepted the node position of a neighbor node " ++ show aNodeId ++
                    " a new position is a " ++ show aNodePosition
                updateFile (NodeInfoListLogicLvl [(aNodeId, aNodePosition)])
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info
                    "Updating of node positions file."
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info
                    "Send to sharding lvl the real coordinate of neighbor"
                sendToShardingLvl aData $
                    T.TheNodeHaveNewCoordinates aNodeId aNodePosition

            -- Accepted info about a live status of neighbor node.
            TheNodeIsAlive _ ok -> do
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
                    "Accepted info about a live status of neighbor node." ++
                    show aNodeId ++ " alive status is a " ++ show ok
                unless ok $ do
                    writeLog (aData^.infoMsgChan) [NetLvlTag] Info
                        "The neighbor is dead send msg about it to the sharding lvl."
                    sendToShardingLvl aData $ T.TheNodeIsDead aNodeId
            _       -> return ()


instance Processing (IORef ManagerNodeData) (Request LogicLvl) where
    processing _ aMd aSignature@(PackageSignature (toNodeId -> aNodeId) _ _) aTraceRouting aRequestLogicLvl = do
        aData <- readIORef aMd
        let aRequestPackage = RequestLogicLvlPackage aRequestLogicLvl aSignature

            aRequestToNetLvl :: (a -> Responce LogicLvl) -> IO a -> IO ()
            aRequestToNetLvl = requestToNetLvl aData aTraceRouting aRequestPackage

        case aRequestLogicLvl of
            ShardIndexRequestPackage _ aDistance -> do
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
                    "Sending request of shard index to sharding lvl. "
                    ++ "Petitioner " ++ show aNodeId ++ " a domain = "
                    ++ show aDistance ++ "."

                aRequestToNetLvl ShardIndexResponce $ do
                    aChan <- newChan
                    sendToShardingLvl aData $
                        T.ShardIndexCreateAction aChan aNodeId aDistance
                    T.ShardIndexResponse aShardIndex <- readChan aChan

                    writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
                        "Recived response from sharding lvl. The sharding index for "
                        ++ show aNodeId ++ " is " ++ show aShardIndex ++ "."
                        ++ " Sending the sharding index."

                    return aShardIndex

            ShardRequestPackage aShardHash -> do
                writeLog (aData^.infoMsgChan) [NetLvlTag, LoadingShardsTag] Info $
                    "Sending request of shard to sharding lvl. "
                    ++ "Petitioner " ++ show aNodeId ++ " shardHash = "
                    ++ show aShardHash ++ "."
                aRequestToNetLvl ShardResponce $ do
                    aChan <- newChan
                    sendToShardingLvl aData $
                        T.ShardLoadAction aChan aNodeId aShardHash
                    T.ShardResponse aShard <- readChan aChan
                    writeLog (aData^.infoMsgChan) [NetLvlTag, LoadingShardsTag] Info $
                        "Recived response from sharding lvl. The shard for "
                        ++ show aNodeId ++ " is " ++ show aShard ++ "."
                        ++ " Sending the shard."
                    return aShard

            NodePositionRequestPackage -> do
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
                    "Sending request of node position to sharding lvl. "
                    ++ "Petitioner " ++ show aNodeId ++ "."
                aRequestToNetLvl NodePositionResponcePackage $ do
                    aChan <- newChan
                    sendToShardingLvl aData $
                        T.NodePositionAction aChan aNodeId
                    T.NodePositionResponse aMyNodePosition <- readChan aChan
                    modifyIORef aMd (myNodePosition ?~ aMyNodePosition)
                    return aMyNodePosition

            IsAliveTheNodeRequestPackage aId -> do
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
                    "Recived request of alive status the node " ++ show aId
                    ++ " from the " ++ show aNodeId ++ "."
                let aMaybeNodeId = case aTraceRouting of
                        ToNode _ (PackageSignature (toNodeId -> aAliveId) _ _)
                            -> Just aAliveId
                        _   -> Nothing

                whenJust aMaybeNodeId $ \aJustNodeId -> do
                    let aNetLvlPackage = TheNodeIsAlive
                            aNodeId . isJust $ aData^.nodes.at aNodeId
                    writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
                        "Send the " ++ show aNetLvlPackage ++ " to "
                        ++ show aJustNodeId ++ "."
                    aResponsePackageSignature <- makePackageSignature aData
                        (aNetLvlPackage, aRequestPackage)
                    let aPackage = ResponceLogicLvlPackage
                            aRequestPackage aNetLvlPackage aResponsePackageSignature
                    aTrace <- makeTraceRouting  aData aPackage
                        (ToNode aJustNodeId)
                    sendResponse (aData^.nodes.at aJustNodeId) aTrace aPackage
            _       -> return ()

instance Processing (IORef ManagerNodeData) (Request NetLvl) where
    processing _ aMd aSignature aTraceRouting aRequest = do
        aData <- readIORef aMd
        let aSendNetLvlResponse = sendNetLvlResponse
                aTraceRouting aData aRequest aSignature
        writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
            "Recived the  " ++ show aRequest  ++ "."
        case aRequest of
            IsYouBrodcast -> do
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
                    "Send responce: I am broadcast " ++ show (aData^.iAmBroadcast)
                    ++ "."
                aSendNetLvlResponse (IAmBroadcast $ aData^.iAmBroadcast)

            HostAdressRequest -> do
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
                    "Send responce: I have host addres " ++ show (aData^.hostAddress) ++ "."
                aSendNetLvlResponse (HostAdressResponce $ aData^.hostAddress)

            BroadcastListRequest -> do
                -- TEMP Think about move aBroadcastList to operacety memory.
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info
                    "Send responce 'Broadcast list'."
                NodeInfoListNetLvl   aBroadcastList      <- readRecordsFromNodeListFile
                NodeInfoListLogicLvl aBroadcastListLogic <- readRecordsFromNodeListFile
                let aBroadcastListResponce = BroadcastListResponce
                        (NodeInfoListLogicLvl $ take 10 aBroadcastListLogic)
                        (NodeInfoListNetLvl   $ take 10 aBroadcastList)

                -- шлём ответ через сеть.
                -- +
                aSendNetLvlResponse aBroadcastListResponce


instance Processing (IORef ManagerNodeData) (Request MiningLvl) where
    processing _ aMd _ _ aRequest = do
        aData <- readIORef aMd
        case aRequest of
            PPMessage aByteString (IdFrom aUuidFrom) (IdTo aId)
                | Just aNode <- aData^.ppNodes.at aId ->
                    writeChan (aNode^.ppChan) $ MsgMsgToPP aUuidFrom aByteString
                | otherwise -> writeLog (aData^.infoMsgChan) [NetLvlTag] Warning $
                    "This PP does not exist: " ++ show aId

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
        | otherwise -> (getClosedNodeByDirect aData (toPoint aPointTo) False, cleanTrace aTrace)
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
    ToNode aNodeId (PackageSignature (toNodeId -> aId) aTime aSig) ->
        ToNode aId (PackageSignature (toMyNodeId aNodeId) aTime aSig)


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
