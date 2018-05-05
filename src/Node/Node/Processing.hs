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
import              Data.List.Extra()
import              Data.IORef
import              System.Random
import              Lens.Micro
import              Lens.Micro.Mtl
import              Control.Concurrent
import              Control.Monad.Extra
import              Crypto.Error()
import              Service.Network.Base

import              Node.Node.Base
import              Node.Node.Types
import              Node.Crypto()
import              Node.Data.Data()
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

instance Processing (IORef ManagerNodeData) (Response MiningLvl) where
    processing _ aMd _ _ = \case
        ResponsePPConnection aUuid (Connect aHost aOutPort) -> do
            aData <- readIORef aMd
            whenJust (aData^.ppNodes.at aUuid) $ \aPpNode ->
                writeChan (aPpNode^.ppChan) $ MsgConnect aHost aOutPort


-- | Обработка "ответа" для сетевого уровня.
instance Processing (IORef ManagerNodeData) (Response NetLvl) where
    processing aChan aMd (PackageSignature (toNodeId -> aNodeId) _ _) _ = \case
        BroadcastListResponse aBroadcastListLogic aBroadcastList -> do
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

        HostAdressResponse _ -> return ()

        -- нода сказала, что она бродкаст меняем её статус в нашей памяти.
        IAmBroadcast          aBool          -> do
            aData <- readIORef aMd
            writeLog (aData^.infoMsgChan) [NetLvlTag] Info $"Node " ++ show aNodeId
                ++ " talk that it is broadcast. Changing the node status."
            modifyIORef aMd $ nodes %~ M.adjust (isBroadcast .~ aBool) aNodeId


-- обработка ответов для логического уровня.
instance Processing (IORef ManagerNodeData) (Response LogicLvl) where
    processing _ aMd (PackageSignature (toNodeId -> aNodeId) _ _) _ aResponse = do
        aData <- readIORef aMd
        case aResponse of
            -- Accepted the index of shards from the neighbor node.
            ShardIndexResponse aShardHashList -> do
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
                    "Send from netLvl to sharding lvl the shards hashes list. "
                    ++ show aShardHashList
                sendToShardingLvl aData $ T.ShardIndexAcceptAction aShardHashList

            -- Accepted the shard from the neighbor node.
            ShardResponse      aShardes         -> do
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
                    "Send from netLvl toSharding lvl the shards acepted from neighbor "
                    ++ "the list of shards is:" ++ show aShardes
                forM_ aShardes $ sendToShardingLvl aData . T.ShardAcceptAction

            -- Accepted the node position of a neighbor node.
            NodePositionResponsePackage (toNodePosition -> aNodePosition) -> do
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

            aRequestToNetLvl :: (a -> Response LogicLvl) -> IO a -> IO ()
            aRequestToNetLvl = requestToNetLvl aData aTraceRouting aRequestPackage

        case aRequestLogicLvl of
            ShardIndexRequestPackage _ aDistance -> do
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
                    "Sending request of shard index to sharding lvl. "
                    ++ "Petitioner " ++ show aNodeId ++ " a domain = "
                    ++ show aDistance ++ "."

                aRequestToNetLvl ShardIndexResponse $ do
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
                aRequestToNetLvl ShardResponse $ do
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
                aRequestToNetLvl NodePositionResponsePackage $ do
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
                    let aPackage = ResponseLogicLvlPackage
                            aRequestPackage aNetLvlPackage aResponsePackageSignature
                    aTrace <- makeTraceRouting  aData aPackage
                        (ToNode aJustNodeId)
                    sendResponse (aData^.nodes.at aJustNodeId) aTrace aPackage
            _       -> return ()

instance Processing (IORef ManagerNodeData) (Request NetLvl) where
    processing _ aMd aSignature aTraceRouting aRequest = do
        aData <- readIORef aMd
        let aSendNetLvlResponse = sendResponseTo
                aTraceRouting aData aRequest aSignature
        writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
            "Recived the  " ++ show aRequest  ++ "."
        case aRequest of
            IsYouBrodcast -> do
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
                    "Send Response: I am broadcast " ++ show (aData^.iAmBroadcast)
                    ++ "."
                aSendNetLvlResponse (IAmBroadcast $ aData^.iAmBroadcast)

            HostAdressRequest -> do
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
                    "Send Response: I have host addres " ++ show (aData^.hostAddress) ++ "."
                aSendNetLvlResponse (HostAdressResponse $ aData^.hostAddress)

            BroadcastListRequest -> do
                -- TEMP Think about move aBroadcastList to operacety memory.
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info
                    "Send Response 'Broadcast list'."
                NodeInfoListNetLvl   aBroadcastList      <- readRecordsFromNodeListFile
                NodeInfoListLogicLvl aBroadcastListLogic <- readRecordsFromNodeListFile
                let aBroadcastListResponse = BroadcastListResponse
                        (NodeInfoListLogicLvl $ take 10 aBroadcastListLogic)
                        (NodeInfoListNetLvl   $ take 10 aBroadcastList)

                -- шлём ответ через сеть.
                -- +
                aSendNetLvlResponse aBroadcastListResponse


instance Processing (IORef ManagerNodeData) (Request MiningLvl) where
    processing _ aMd aSignature aTraceRouting aRequest = do
        aData <- readIORef aMd
        case aRequest of
            PPMessage aByteString (IdFrom aUuidFrom) (IdTo aId)
                | Just aNode <- aData^.ppNodes.at aId ->
                    writeChan (aNode^.ppChan) $ MsgMsgToPP aUuidFrom aByteString
                | otherwise -> writeLog (aData^.infoMsgChan) [NetLvlTag] Warning $
                    "This PP does not exist: " ++ show aId
            RequestPPConnection aUuid -> whenJust (aData^.hostAddress) $ \aHost -> do
                let aResponse = sendResponseTo
                        aTraceRouting aData aRequest aSignature
                aResponse $ ResponsePPConnection aUuid (Connect aHost (aData^.outPort))


sendToShardingLvl :: ManagerData md => md -> T.ShardingNodeAction -> IO ()
sendToShardingLvl aData aMsg = whenJust (aData^.shardingChan) $ \aChan ->
    writeChan aChan aMsg


requestToNetLvl
    ::  ManagerNodeData
    ->  TraceRouting
    ->  RequestPackage
    -> (a -> Response LogicLvl)
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
            (ResponseLogicLvlPackage aRequestPackage aNetLevetPackage aResponsePackageSignature)


--------------------------------------------------------------------------------
