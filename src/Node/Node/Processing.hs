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
module Node.Node.Processing (
        Processing(..)
    ,   sendToShardingLvl
  ) where

import qualified    Data.Map                        as M
import              Data.List.Extra()
import              Data.IORef
import              System.Random()
import              Lens.Micro
import              Lens.Micro.Mtl()
import qualified    Control.Concurrent as C
import              Control.Concurrent.Chan.Unagi.Bounded
import              Control.Monad.Extra
import              Service.Network.Base

import              Node.Node.Types
import              Node.Data.NetPackage
import              Sharding.Sharding()
import qualified    Sharding.Types.Node as T
import              Control.Concurrent.MVar
import              Sharding.Space.Point
import              Node.Data.MakeAndSendTraceRouting
import              Node.Data.MakeTraceRouting
import              Node.Data.GlobalLoging
import              Service.InfoMsg
import              Data.Maybe
import              PoA.Types
import              Node.Data.Key
import              Node.FileDB.FileServer

class Processing aNodeData aPackage where
    processing
        ::  ManagerMsg msg
        =>  InChan msg
        ->  aNodeData
        ->  PackageSignature
        ->  TraceRouting
        ->  aPackage
        ->  IO ()

instance Processing (IORef ManagerNodeData) (Response MiningLvl) where
    processing _ aMd _ _ = \case
        ResponsePPConnection aPPId (Connect aHost aOutPort) -> do
            aData <- readIORef aMd
            whenJust (aData^.ppNodes.at aPPId) $ \aPpNode ->
                writeChan (aPpNode^.ppChan) $ MsgConnect aHost aOutPort


-- | Handling network layer's answer
instance Processing (IORef ManagerNodeData) (Response NetLvl) where
    processing _ aMd (PackageSignature (toNodeId -> aNodeId) _ _) _ = \case
        BroadcastListResponse aBroadcastListLogic aBroadcastList _ -> do
            aData <- readIORef aMd
            writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
                "Accepted lists of broadcasts and points of node. " ++
                show aBroadcastListLogic ++ show aBroadcastList
{-
            writeChan (aData^.fileServerChan) $
                FileActorRequestNetLvl $ UpdateFile (aData^.myNodeId) aBroadcastList

            writeChan (aData^.fileServerChan) $
                FileActorRequestLogicLvl $ UpdateFile (aData^.myNodeId) aBroadcastListLogic
-}
        HostAdressResponse _ -> return ()

        -- node report, that it is broadcast node. Change its status in our memory.
        IAmBroadcast          aBool          -> do
            aData <- readIORef aMd
            writeLog (aData^.infoMsgChan) [NetLvlTag] Info $"Node " ++ show aNodeId
                ++ " talk that it is broadcast. Changing the node status."
            modifyIORef aMd $ nodes %~ M.adjust (isBroadcast .~ aBool) aNodeId


-- Logical layer answers' handling
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
                {-
                writeChan (aData^.fileServerChan) $
                    FileActorRequestLogicLvl $ UpdateFile (aData^.myNodeId)
                        (NodeInfoListLogicLvl [(aNodeId, aNodePosition)])
                -}
                whenJust (aData^.nodes.at aNodeId) $ \aNode ->
                    modifyIORef aMd $ nodes %~ M.insert aNodeId
                        (aNode & nodePosition ?~ aNodePosition)
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
                    aChan <- C.newChan
                    sendToShardingLvl aData $
                        T.ShardIndexCreateAction aChan aNodeId aDistance
                    T.ShardIndexResponse aShardIndex <- C.readChan aChan

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
                    aChan <- C.newChan
                    sendToShardingLvl aData $
                        T.ShardLoadAction aChan aNodeId aShardHash
                    T.ShardResponse aShard <- C.readChan aChan
                    writeLog (aData^.infoMsgChan) [NetLvlTag, LoadingShardsTag] Info $
                        "Recived response from sharding lvl. The shard for "
                        ++ show aNodeId ++ " is " ++ show aShard ++ "."
                        ++ " Sending the shard."
                    return aShard

            NodePositionRequestPackage -> do
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
                    "Sending request of node position to sharding lvl. "
                    ++ "Pesitioner " ++ show aNodeId ++ "."
                aRequestToNetLvl NodePositionResponsePackage $ do
                    aChan <- C.newChan
                    sendToShardingLvl aData $
                        T.NodePositionAction aChan aNodeId
                    T.NodePositionResponse aMyNodePosition <- C.readChan aChan
                    writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
                        "Accepted responce of node position from sharding lvl. "
                        ++ show aMyNodePosition
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

            BroadcastListRequest -> void $ C.forkIO $ do
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info
                    "Send Response 'Broadcast list'."
{-
                aPosChan <- newEmptyMVar
                aConChan <- newEmptyMVar
                writeChan (aData^.fileServerChan) $
                     FileActorRequestNetLvl $ ReadRecordsFromNodeListFile aConChan
                writeChan (aData^.fileServerChan) $
                     FileActorRequestLogicLvl $ ReadRecordsFromNodeListFile aPosChan

                NodeInfoListNetLvl   aBroadcastList      <- takeMVar aConChan
                NodeInfoListLogicLvl aBroadcastListLogic <- takeMVar aPosChan
-}
                let aBroadcastListResponse = BroadcastListResponse
                        (NodeInfoListLogicLvl [])
                        (NodeInfoListNetLvl   [])
                        False

                aSendNetLvlResponse aBroadcastListResponse


instance Processing (IORef ManagerNodeData) (Request MiningLvl) where
    processing _ aMd aSignature aTraceRouting aRequest = do
        aData <- readIORef aMd
        case aRequest of
            PPMessage aByteString (IdFrom aPPIdFrom) (IdTo aId)
                | Just aNode <- aData^.ppNodes.at aId ->
                    writeChan (aNode^.ppChan) $ MsgMsgToPP aPPIdFrom aByteString
                | otherwise -> writeLog (aData^.infoMsgChan) [NetLvlTag] Warning $
                    "This PP does not exist: " ++ show aId
            RequestPPConnection aPPId -> whenJust (aData^.hostAddress) $ \aHost -> do
                let aResponse = sendResponseTo
                        aTraceRouting aData aRequest aSignature
                aResponse $ ResponsePPConnection aPPId (Connect aHost (aData^.outPort))


sendToShardingLvl :: ManagerData md => md -> T.ShardingNodeAction -> IO ()
sendToShardingLvl aData aMsg = do
    writeLog (aData^.infoMsgChan) [NetLvlTag] Info "Sending msg to sharding lvl"
    whenJust (aData^.shardingChan) $ \aChan -> do
        writeLog (aData^.infoMsgChan) [NetLvlTag] Info "Sended msg to sharding lvl"
        C.writeChan aChan aMsg


requestToNetLvl
    ::  ManagerNodeData
    ->  TraceRouting
    ->  RequestPackage
    -> (a -> Response LogicLvl)
    ->  IO a
    ->  IO ()
requestToNetLvl aData aTraceRouting aRequestPackage aConstructor aLogicRequest =
    void $ C.forkIO $ do
        aResultOfRequest <- aLogicRequest
        let (aNode, aTrace) = getClosedNode aTraceRouting aData
            aNetLevetPackage = aConstructor aResultOfRequest

        aResponsePackageSignature <- makePackageSignature aData
            (aNetLevetPackage, aRequestPackage)

        sendResponse aNode
            (makeNewTraceRouting aTrace aTraceRouting)
            (ResponseLogicLvlPackage aRequestPackage aNetLevetPackage aResponsePackageSignature)


--------------------------------------------------------------------------------
