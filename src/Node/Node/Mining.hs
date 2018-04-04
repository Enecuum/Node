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

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Node.Node.Mining where

import qualified    Crypto.PubKey.ECC.ECDSA         as ECDSA

import qualified    Data.Set                        as S
import qualified    Data.ByteString                 as B
import qualified    Data.Map                        as M
import qualified    Data.Bimap                      as BI
import              Data.Maybe
import              Service.Types.PublicPrivateKeyPair
import              Data.List.Extra
import              Service.Types
import              System.Clock
import              System.Random.Shuffle
import              Data.IORef
import              Data.Serialize
import              Lens.Micro
import              Control.Concurrent
import              Control.Concurrent.Chan
import              Control.Monad.Extra
import              Node.Node.Base
import              Node.Node.Types
import              Service.Monad.Option
import              Node.Crypto
import              Service.Metrics

import              Node.Data.NodeTypes
import              Node.Data.NetPackage
import              Node.Data.NetMesseges
import              Node.Action.NetAction
import qualified    Sharding.Types.Node as T
import              Sharding.Space.Point
import              Sharding.Space.Distance


managerMining :: Chan ManagerMiningMsgBase -> IORef ManagerNodeData -> IO ()
managerMining ch aMd = forever $ do
    mData <- readIORef aMd
    readChan ch >>= \a -> runOption a $ do
        baseNodeOpts ch aMd mData

        opt isInitDatagram          $ answerToInitDatagram aMd
        opt isDatagramMsg           $ answerToDatagramMsg ch aMd (mData^.myNodeId)
        opt isClientIsDisconnected $ miningNodeAnswerClientIsDisconnected aMd

--      opt isNewTransaction            $ answerToNewTransaction aMd
        opt isInitDatagram              $ answerToSendInitDatagram ch aMd
--      opt isBlockMadeMsg              $ answerToBlockMadeMsg aMd
        opt isDeleteOldestMsg           $ answerToDeleteOldestMsg aMd
        opt isDeleteOldestVacantPositions $ answerToDeleteOldestVacantPositions aMd

miningNodeAnswerClientIsDisconnected ::
    IORef ManagerNodeData -> ManagerMiningMsgBase -> IO ()
miningNodeAnswerClientIsDisconnected aMd
    (toManagerMsg -> ClientIsDisconnected aNodeId aChan) = do
        aData <- readIORef aMd
        loging aData $ "miningNodeAnswerClientIsDisconnected " ++ show aNodeId
        whenJust (aNodeId `M.lookup`(aData^.nodes)) $ \aNode -> do
            when (aChan == (aNode^.chan)) $ do
                minusStatusNumber aMd aNodeId
                modifyIORef aMd $ (nodes %~ M.delete aNodeId)
miningNodeAnswerClientIsDisconnected _ _ = pure ()

----TODO: MOVE TO ?????? --------------
answerToDeleteOldestVacantPositions ::
    IORef ManagerNodeData -> ManagerMiningMsgBase -> IO ()
answerToDeleteOldestVacantPositions aMd _ = do
    aTime <- getTime Realtime
    modifyIORef aMd $ vacantPositions %~ BI.filter (\aTimeSpec _ ->
        diffTimeSpec aTime aTimeSpec > 3000000)

----TODO: MOVE TO ?????? --------------
answerToDeleteOldestMsg :: IORef ManagerNodeData -> ManagerMiningMsgBase -> IO ()
answerToDeleteOldestMsg aMd _ = do
    aTime <- getTime Realtime
    modifyIORef aMd $ hashMap %~ deleteOldest aTime


instance BroadcastAction ManagerNodeData where
    makeBroadcastAction aChan aMd aNodeId aBroadcastSignature aBroadcastThing = do
        aData <- readIORef aMd
        loging aData $ "BroadcastAction ManagerNodeData" ++ show aBroadcastThing
        when (notInIndex aData aBroadcastThing) $ do
            addInIndex aBroadcastThing aMd
            sendBroadcastThingToNodes aMd aBroadcastSignature aBroadcastThing
            processingOfBroadcastThing aMd aBroadcastThing

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
{-
class Verifycation a where
    verifyPackage :: a -> Bool

instance Verifycation ResponcePackage where
    verifyPackage = \case
        ResponceNetLvlPackage aRequestPackage aSignature aResponse ->
            verifyEncodeble  ()
            | verifyNetLvlResponse aRequestPackage aSignature aResponse ->
                True
        ResponceLogicLvlPackage aRequestPackage aSignature aResponse ->
            | verifyLogicLvlResponse aRequestPackage aSignature aResponse ->

-}

instance PackageTraceRoutingAction ManagerNodeData ResponcePackage where
    makeAction _ md aNodeId aTraceRouting aResponcePackage = do
        aData <- readIORef md
        when (verifyResponce aTraceRouting aResponcePackage) $ if
            | isItMyResponce aNodeId aTraceRouting  -> aProcessingOfAction aData
            | otherwise                             -> aSendToNeighbor aData
      where
        verifyResponce _ _ = True -- TODO : add body
        aProcessingOfAction aData = case aResponcePackage of
            ResponceNetLvlPackage aResponse aSignature | True ->
                processing md aSignature aTraceRouting aResponse
            ResponceLogicLvlPackage aResponse aSignature | True ->
                processing md aSignature aTraceRouting aResponse

        aSendToNeighbor aData = do
            let (aNode, aNewTrace) = getClosedNode aTraceRouting aData
                aMaybePoints = case aTraceRouting of
                    ToDirect aPointFrom aPointTo _
                        -> Just (aPointFrom, aPointTo)
                    _   -> Nothing
            whenJust aMaybePoints $ \(aPointFrom, aPointTo) ->
                whenJust aNode $ sendToNode (makeResponse
                    (ToDirect aPointFrom aPointTo aNewTrace) aResponcePackage)


closedToPointNeighbor aData aPointTo = sortOn
    (\n -> distanceTo n aPointTo)
        $ M.elems $ aData^.nodes

amIClose aData aNode aPointTo = if
    | Just aPosition <- aData^.myNodePosition,
        distanceTo aPosition aPointTo < distanceTo aNode aPointTo -> True
    | otherwise -> False


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


signatureToNodeId :: PackageSignature -> NodeId
signatureToNodeId (PackageSignature (toNodeId -> aNodeId) _ _) = aNodeId


instance DistanceTo Node Point where
    distanceTo aNode aPoint = if
        | Just aPosition <- aNode^.nodePosition ->
            distanceTo aPosition  (NodePosition aPoint)
        | otherwise                             -> maxBound

instance DistanceTo Node PointTo where
    distanceTo aNode aPoint = distanceTo aNode (toPoint aPoint)



traceDrop aNextNodeId = dropWhile
    (\(PackageSignature (toNodeId -> aId) _ _) -> aId /= aNextNodeId)


isItMyResponce :: NodeId -> TraceRouting -> Bool
isItMyResponce aMyNodeId = \case
    ToNode   _ (PackageSignature aNodeId _ _)
        | toNodeId aNodeId == aMyNodeId -> True
    ToDirect _ _ (last -> (PackageSignature (toNodeId -> aNodeId) _ _))
        | aNodeId == aMyNodeId          -> True
    _                                   -> False

instance Processing (IORef ManagerNodeData) RequestLogicLvl where
    processing aMd (PackageSignature (toNodeId -> aNodeId) _ _) aTraceRouting aRequestLogicLvl= do
        aData <- readIORef aMd
        case aRequestLogicLvl of
            ShardIndexRequestPackage _ aDistance ->
                void $ forkIO $ do
                    aChan <- newChan
                    sendToShardingLvl aData $ T.ShardIndexCreateAction aChan aNodeId aDistance
                    T.ShardIndexResponse aShardListHash <- readChan aChan
                    whenJust (aNodeId `M.lookup` (aData^.nodes)) $
                        sendToNode (makeRequest undefined undefined)

{-
            ShardRequestPackage aShardHash -> sendToShardingLvl aData $
                T.ShardListCreateAction aNodeId [aShardHash]
                -}
            NodePositionRequestPackage ->
                whenJust (aData^.myNodePosition) $ \aMyPosition ->
                    whenJust (aNodeId `M.lookup` (aData^.nodes)) $
                        sendToNode (makeRequest undefined undefined)

                      ---  ShardIndexResponse :: NodeId -> [ShardHash] -> ShardingNodeResponce
                      ---  ShardListResponse  :: NodeId -> [Shard]     -> ShardingNodeResponce


{-
        BroadcastListResponce aBroadcastList -> do
            aData <- readIORef aMd
            forM_ aBroadcastList $ \(aNodeId, aIp, aPort) -> do
                addRecordToNodeListFile (aData^.myNodeId) aNodeId aIp aPort

        HostAdressResponce    aHostAdress    -> return ()

        IAmBroadcast          aBool          -> do
            modifyIORef aMd $ nodes %~ M.adjust (isBroadcast .~ aBool) aNodeId
--

NewNodeInNetAction          NodeId NodePosition
-- TODO create index for new node by NodeId
|   ShardIndexCreateAction      NodeId Word64
|   ShardIndexAcceptAction      [ShardHash]
|   ShardListCreateAction       NodeId [ShardHash]
|   ShardAcceptAction           Shard
---
|   NewShardInNetAction         Shard
|   CleanShardsAction -- clean local Shards
--- ShiftAction => NewPosiotionResponse
|   ShiftAction
|   TheNodeHaveNewCoordinates   NodeId NodePosition
---- NeighborListRequest => NeighborListAcceptAction
|   TheNodeIsDead               NodeId

--

NodePositionRequestPackage  ::                                     RequestLogicLvl
-}

---------------TODO: fix True---------------------------------------------------
instance PackageTraceRoutingAction ManagerNodeData RequestPackage where
    makeAction _ md aNodeId aTraceRouting aRequestPackage = do
        aData <- readIORef md
        when True $ if
            | True                                 -> aProcessingOfAction aData
            | otherwise                            -> aSendToNeighbor aData
      where
        aProcessingOfAction aData = case aRequestPackage of
            RequestLogicLvlPackage aRequest aSignature
                | True -> processing md aSignature aTraceRouting aRequest
            RequestNetLvlPackage aRequest aSignature
                | True -> processing md aSignature aTraceRouting aRequest

        aSendToNeighbor aData = case aTraceRouting of
            ToDirect aPointFrom aPointTo aSignatures ->
                whenJust (getClosedNodeByDirect aData (toPoint aPointTo)) $
                    \aNode -> do
                        aNewTrace <- addToTrace
                            aTraceRouting
                            aRequestPackage
                            (aData^.myNodeId)
                            (aData^.privateKey)
                        sendToNode (makeRequest aNewTrace aRequestPackage) aNode
            _ -> return ()


addToTrace :: TraceRouting -> RequestPackage -> MyNodeId -> ECDSA.PrivateKey -> IO TraceRouting
addToTrace aTraceRouting aRequestPackage aMyNodeId aPrivateKey = do
    aTime     <- getTime Realtime
    aSignature <- signEncodeble aPrivateKey
        (aTraceRouting, aRequestPackage, aMyNodeId, aTime)
    let aPackageSignature = PackageSignature aMyNodeId aTime aSignature
    case aTraceRouting of
        ToDirect aPointFrom aPointTo aSignatures ->
            return $ ToDirect aPointFrom aPointTo (aPackageSignature : aSignatures)
        _ -> error "Node.Node.Mining.addToTrace: It is not ToDirect!"

        {-

        BroadcastListRequest                        -> do
            aData <- readIORef md
            whenJust (aNodeId `M.lookup` (aData^.nodes)) $ \aNode -> do
                aRawListOfContacts <- readRecordFromNodeListFile $ aData^.myNodeId
                aListOfContacts    <- shuffleM aRawListOfContacts
                sendToNode
                    (makeResponse aTraceRouting
                        (BroadcastListResponce BroadcastListRequest
                            (take 5 aListOfContacts)))
                    aNode
-}

makeRequest aTraceRouting aRequest = makeCipheredPackage
    (PackageTraceRoutingRequest aTraceRouting aRequest)

makeResponse aTraceRouting aResponse = makeCipheredPackage
    (PackageTraceRoutingResponce aTraceRouting aResponse)

{-
instance NetAction ManagerNodeData where

    actionByPing _ aMd aNodeId aPing = do
        aData <- readIORef aMd
        loging aData $ "miningNodeAnswerToPing" ++ show aPing
        case aPing of
            IPRequest aTimeSpec aSignature -> whenJust (aNodeId `M.lookup` (aData^.nodes)) $
                \aNode -> sendToNode
                    (makePingPongMsg Pong (IPAnswer (aNode^.nHostAddress) aTimeSpec aSignature))
                    aNode

            BroadcastNodeListRequest -> whenJust (aNodeId `M.lookup` (aData^.nodes)) $
                \aNode -> do
                    aRawListOfContacts <- readRecordFromNodeListFile $ aData^.myNodeId
                    aListOfContacts    <- shuffleM $ map (\(a, b, c) -> (a, (b, c)))
                        aRawListOfContacts
                    sendToNode
                        (makePingPongMsg Pong (BroadcastNodeListAnswer $ take 5 aListOfContacts))
                        aNode
            _ -> return ()

    actionByPong _ aMd aId aPong = do
        aData <- readIORef aMd
        loging aData $ "miningNodeAnswerToPong " ++ show aPong
        case aPong of
            aIPAnswer@(IPAnswer aIp aTimeSpec _)
                | verifyIPAnswer aId (aData^.publicKey) aIPAnswer -> do
                    aTime <- getTime Realtime
                    when (diffTimeSpec aTime aTimeSpec < 3000000000) $
                        modifyIORef aMd $ nodeBaseData.hostAddress .~ Just aIp

            BroadcastNodeListAnswer aListAnswer ->
                forM_ aListAnswer $ \(aNodeId, (aIp, aPort)) ->
                    addRecordToNodeListFile (aData^.myNodeId) aNodeId aIp aPort
            _                                   -> pure ()
    --

    actionByInfoPing _ aMd _ aInfoPing = do
        aData <- readIORef aMd
        loging aData $ "miningNodeAnswerToInfoPing " ++ show aInfoPing
        when (notInIndex aData aInfoPing) $ do
            addInIndex aInfoPing aMd
            sendInfoPingToNodes aMd aInfoPing
            processingOfInfoPing aMd aInfoPing

    actionByRequest _ aMd _ _ aRequest = do
        aData <- readIORef aMd
        loging aData $ "miningNodeAnswerToInfoPing " ++ show aRequest
        when (notInIndex aData aRequest) $ do
            addInIndex aRequest aMd
-}
{-
            sendInfoPingToNodes aMd aInfoPing
            processingOfInfoPing aMd aInfoPing
-}

{-
class NetAction aNodeType where

    actionByRequest         :: ManagerData md => aNodeType -> ShardingAction t md RequestPackage
    actionByAnswerMsg       :: ManagerData md => aNodeType -> ShardingAction t md AnswerPackage
    actionByConfirmRequest  :: ManagerData md => aNodeType -> ShardingAction t md ConfirmationOfRequestPackage

-}
{-

answerToNewTransaction :: IORef ManagerNodeData -> ManagerMiningMsgBase -> IO ()
answerToNewTransaction aMd (NewTransaction aTransaction) = do
    metric $ increment "net.tx.count"
    aData <- readIORef aMd
    loging aData $ "new transaction in net"
    addInIndex (NewTransactionInNet aTransaction) aMd
    sendInfoPingToNodes aMd $ NewTransactionInNet aTransaction
    metric $ add
        ("net.node." ++ show (toInteger $ aData^.myNodeId) ++ ".pending.amount")
        (1 :: Integer)
    writeChan (aData^.transactions) aTransaction
answerToNewTransaction _ _ = error
    "answerToNewTransaction: something unexpected  has happened."

-}
{-
answerToBlockMadeMsg :: ManagerMiningMsg msg =>
    IORef ManagerNodeData -> msg -> IO ()
answerToBlockMadeMsg aMd (toManagerMiningMsg -> BlockMadeMsg aMicroblock) = do
    metric $ increment "net.bl.count"
    aData <- readIORef aMd
    loging aData $ "answerToBlockMadeMsg " ++ show aMicroblock

    addInIndex (BlockMade aMicroblock) aMd
    sendInfoPingToNodes aMd $ BlockMade aMicroblock
    writeChan (aData^.microblockChan) aMicroblock
answerToBlockMadeMsg _ _ = pure ()

-}


isBrodcastNode :: ManagerNodeData -> Bool
isBrodcastNode aData = aData^.nodeConfig.helloMsg.nodeVariantRoles.to
    (BroadcastNode `elem`)

notInIndex :: Serialize a => ManagerNodeData -> a -> Bool
notInIndex aData a = not $ BI.memberR (cryptoHash a) $ aData^.hashMap


addInIndex :: Serialize a => a -> IORef ManagerNodeData -> IO ()
addInIndex aMsg aMd = do
    aTime <- getTime Realtime
    modifyIORef aMd $ hashMap %~ BI.insert aTime (cryptoHash aMsg)

deleteOldest ::
    TimeSpec
    -> BI.Bimap TimeSpec B.ByteString
    -> BI.Bimap TimeSpec B.ByteString
deleteOldest aTime = BI.filter
    (\aOldTime _ -> diffTimeSpec aOldTime aTime < fromNanoSecs 3000000)


isBootNode :: NodeId -> ManagerNodeData -> Bool
isBootNode aId aData = aId `elem`
    ((\(i, _, _) -> i) <$> (aData^.nodeBaseData.bootNodes))


eq :: MyNodeId -> NodeId -> Bool
eq (MyNodeId aMyNodeId) (NodeId aNodeId) = aMyNodeId == aNodeId


processingOfBroadcastThing :: IORef ManagerNodeData -> BroadcastThing -> IO ()
processingOfBroadcastThing aMd aBroadcastThing = do
    aData <- readIORef aMd
    loging aData $ "Recived " ++ show aBroadcastThing
    case aBroadcastThing of
        BroadcastWarning      aBroadcastWarning -> case aBroadcastWarning of
            INeedNeighbors aMyNodeId aHostAddress   -> undefined
        BroadcastShard        aShard            -> do
            whenJust (aData^.shardingChan) $ \aChan ->
                writeChan aChan $ T.NewShardInNetAction aShard
        BroadcastTransaction  aTransaction      ->
{-
            metric $ add ("net.node." ++ show (toInteger $ aData^.myNodeId) ++ ".pending.amount") (1 :: Integer)
-}
            writeChan (aData^.transactions) aTransaction
        BroadcastPosition     aMyNodeId aNodePosition  -> do
            sendToShardingLvl aData $
                T.TheNodeHaveNewCoordinates (toNodeId aMyNodeId) aNodePosition

sendToShardingLvl aData aMsg = whenJust (aData^.shardingChan) $ \aChan ->
    writeChan aChan aMsg


{-
        aMsg@(IHaveBroadcastConnects _ _ aIp aPort aNodeId _)
            | verifyIHaveBroadcastConnects aMsg ->
                unless ((aData^.myNodeId) `eq` aNodeId) $ do
                    aTime <- getTime Realtime
                    modifyIORef aMd $ vacantPositions %~ BI.insert aTime
                        (aNodeId, aIp, aPort)
                    addRecordToNodeListFile (aData^.myNodeId) aNodeId aIp aPort

        BlockMade aMicroblock -> do
            writeChan (aData^.microblockChan) aMicroblock
-}
