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

import qualified    Data.Map                        as M
import qualified    Data.Bimap                      as BI
import              Data.Maybe (isNothing)
import              Data.List.Extra
import              System.Clock
import              Data.IORef
import              Data.Serialize
import              Lens.Micro
import              Control.Concurrent
import              Control.Monad.Extra
import              Crypto.Error
import              Node.Node.BroadcastProcessing


import              Sharding.Space.Distance
import              Sharding.Types.ShardLogic
import              Service.Monad.Option
import              Node.Crypto
import              Node.Data.Data
import              Node.Node.Types
import              Node.Node.Base
import              Node.Data.NodeTypes
import              Node.Data.NetPackage
import qualified    Sharding.Types.Node as T
import              Sharding.Space.Point
import              Node.Node.Processing
import              Service.InfoMsg
import              Node.Data.MakeAndSendTraceRouting
import              Node.Data.Verification
import              Node.Data.GlobalLoging

managerMining :: Chan ManagerMiningMsgBase -> IORef ManagerNodeData -> IO ()
managerMining ch aMd = forever $ do
    mData <- readIORef aMd
    readChan ch >>= \a -> runOption a $ do
        baseNodeOpts ch aMd mData

        opt isInitDatagram          $ answerToInitDatagram aMd
        opt isDatagramMsg           $ answerToDatagramMsg ch aMd (mData^.myNodeId)
        opt isClientIsDisconnected $ miningNodeAnswerClientIsDisconnected aMd

        opt isNewTransaction            $ answerToNewTransaction aMd
        opt isBlockMadeMsg              $ answerToBlockMadeMsg aMd
        opt isInitDatagram              $ answerToSendInitDatagram ch aMd
        opt isShardingNodeRequestMsg    $ answerToShardingNodeRequestMsg aMd
        opt isDeleteOldestMsg           $ answerToDeleteOldestMsg aMd

miningNodeAnswerClientIsDisconnected
    ::  IORef ManagerNodeData
    ->  ManagerMiningMsgBase
    ->  IO ()
miningNodeAnswerClientIsDisconnected aMd
    (toManagerMsg -> ClientIsDisconnected aNodeId aChan) = do
        aData <- readIORef aMd
        whenJust (aNodeId `M.lookup`(aData^.nodes)) $ \aNode -> do
            writeLog aData $ "The node " ++ show aNodeId ++ " is disconnected."
            when (aChan == (aNode^.chan)) $ do
                modifyIORef aMd $ (nodes %~ M.delete aNodeId)
miningNodeAnswerClientIsDisconnected _ _ = pure ()


answerToShardingNodeRequestMsg
    ::  IORef ManagerNodeData
    ->  ManagerMiningMsgBase
    ->  IO ()
answerToShardingNodeRequestMsg aMd
    (toManagerMsg -> ShardingNodeRequestMsg aNetLvlMsg) = do
        aData <- readIORef aMd
        let aLogMsg a = "Net lvl accept a msg about " ++ a ++  " from logic lvl."
        let aLogAboutAliveRequest aNodeId =
                writeLog aData $ aLogMsg "is this neighbor alive" ++
                    "Noda about which they asked: " ++  show aNodeId
        case aNetLvlMsg of
            T.NewPosiotionMsg aMyNodePosition -> do
                writeLog aData $ aLogMsg "new position"
                    ++ ". The position is "
                    ++ show aMyNodePosition ++ "."
                sendBroadcast aMd
                    (BroadcastLogic $ BroadcastPosition
                        (aData^.myNodeId)
                        (toNodePosition aMyNodePosition))

            T.IamAwakeRequst _ aMyNodePosition -> do
                writeLog aData $ aLogMsg "awake logic lvl"
                sendBroadcast aMd
                    (BroadcastLogic $ BroadcastPosition
                        (aData^.myNodeId)
                        (toNodePosition aMyNodePosition))

            T.NeighborListRequest -> do
                writeLog aData $ aLogMsg "neighbors"
                makeAndSendTo aData (M.keys $ aData^.nodes) $
                    NeighborListRequestPackage

            T.ShardIndexRequest aDistance aNodePositions -> do
                writeLog aData $ aLogMsg "hashes of needed shards"
                whenJust (aData^.myNodePosition) $ \aMyPosition -> do
                    let aRequest = ShardIndexRequestPackage
                            (toNodePosition aMyPosition) aDistance
                    forM_ aNodePositions $ \aPosition -> do
                        makeAndSendTo aData aPosition aRequest

            T.ShardListRequest shardHashes -> do
                writeLog aData $ aLogMsg "needed shards"
                writeLog aData $ "The list of requested shards:" ++ show shardHashes
                forM_ shardHashes $ \aHash -> do
                    let aPosition = NodePosition $ hashToPoint aHash
                        aRequest  = ShardRequestPackage aHash
                    makeAndSendTo aData aPosition aRequest

            T.IsTheNeighborAliveRequest aNodeId aNodePosition
                | aData^.iAmBroadcast -> if
                    | Just _ <- aData^.nodes.at aNodeId -> do
                        aLogAboutAliveRequest aNodeId
                        writeLog aData $ "The node is alive."
                        return ()
                    | otherwise -> do
                        aLogAboutAliveRequest aNodeId
                        writeLog aData $ "The node is dead."
                        sendToShardingLvl aData $ T.TheNodeIsDead aNodeId
                | otherwise -> do
                    let aListOfBroatcastPosition = concat $ do
                            (aId, aNode) <- M.toList $ aData^.nodes
                            if  | Just aPosition <- aNode^.nodePosition
                                    -> return [(aId, aPosition)]
                                | otherwise -> return []

                        aBroadcastNodeId = take 1 $(^._1) <$> sortOn
                            (\a -> distanceTo (a^._2) aNodePosition)
                            aListOfBroatcastPosition
                    aLogAboutAliveRequest aNodeId
                    writeLog aData $ "Request to broadcast node about state (alive or dead) of " ++ show aNodeId
                    makeAndSendTo aData aBroadcastNodeId
                        (IsAliveTheNodeRequestPackage aNodeId)
answerToShardingNodeRequestMsg _ _ = return ()


answerToDeleteOldestMsg
    ::  IORef ManagerNodeData
    ->  ManagerMiningMsgBase
    ->  IO ()
answerToDeleteOldestMsg aMd _ = do
    aData <- readIORef aMd
    writeLog aData "Cleaning of index of bradcasted msg."
    aTime <- getTime Realtime
    modifyIORef aMd $ hashMap %~ BI.filter
        (\aOldTime _ -> diffTimeSpec aOldTime aTime < fromNanoSecs 3000000)


instance BroadcastAction ManagerNodeData where
    makeBroadcastAction _ aMd _ aBroadcastSignature aBroadcastThing = do
        aData <- readIORef aMd
        writeLog aData $ "Recived the broadcast msg " ++ show aBroadcastThing ++ "."
        when (notInIndex aData aBroadcastThing) $ do
            addInIndex aBroadcastThing aMd
            sendBroadcastThingToNodes aMd aBroadcastSignature aBroadcastThing
            processingOfBroadcastThing aMd aBroadcastThing


instance PackageTraceRoutingAction ManagerNodeData ResponcePackage where
    makeAction aChan md aNodeId aTraceRouting aResponcePackage = do
        aData <- readIORef md
        writeLog aData $ "Recived a responce package."
        when (verify (aTraceRouting, aResponcePackage)) $ if
            | isItMyResponce aNodeId aTraceRouting  -> do
                writeLog aData $ "The responce is for me. The processing of responce."
                aProcessingOfAction
            | otherwise -> aSendToNeighbor aData
      where
        aProcessingOfAction = case aResponcePackage of
            ResponceNetLvlPackage _ aResponse aSignature ->
                processing aChan md aSignature aTraceRouting aResponse
            ResponceLogicLvlPackage _ aResponse aSignature ->
                processing aChan md aSignature aTraceRouting aResponse

        aSendToNeighbor aData = do
            writeLog aData $ "This is someone else's message. Resending of responce."
            let (aNode, aNewTrace) = getClosedNode aTraceRouting aData
                aMaybePoints = case aTraceRouting of
                    ToDirect aPointFrom aPointTo _
                        -> Just (aPointFrom, aPointTo)
                    _   -> Nothing
            whenJust aMaybePoints $ \(aPointFrom, aPointTo) -> do
                whenJust aNode $ sendToNode (makeResponse
                    (ToDirect aPointFrom aPointTo aNewTrace) aResponcePackage)


isItMyResponce :: NodeId -> TraceRouting -> Bool
isItMyResponce aMyNodeId = \case
    ToNode   _ (PackageSignature aNodeId _ _)
        | toNodeId aNodeId == aMyNodeId -> True
    ToDirect _ _ (last -> (PackageSignature (toNodeId -> aNodeId) _ _))
        | aNodeId == aMyNodeId          -> True
    _                                   -> False

--  THINK: About request routing.
--  How to understand what the request is for me, if i have the more closed
--  neighbor to the point.
instance PackageTraceRoutingAction ManagerNodeData RequestPackage where
    makeAction aChan md _ aTraceRouting aRequestPackage = do
        aData <- readIORef md
        when (verify (aTraceRouting, aRequestPackage)) $ case aTraceRouting of
            ToDirect _ aPointTo _ -> do
                let aMaybeNode = getClosedNodeByDirect aData (toPoint aPointTo)
                whenJust aMaybeNode $ \aNode -> do
                    aNewTrace <- addToTrace
                        aTraceRouting
                        aRequestPackage
                        (aData^.myNodeId)
                        (aData^.privateKey)
                    sendToNode (makeRequest aNewTrace aRequestPackage) aNode
                when (isNothing aMaybeNode) aProcessingOfAction
            ToNode aNodeId _ | toNodeId (aData^.myNodeId) == aNodeId ->
                aProcessingOfAction
            _   -> return ()

      where
        aProcessingOfAction = case aRequestPackage of
            RequestLogicLvlPackage aRequest aSignature  ->
                processing aChan md aSignature aTraceRouting aRequest
            RequestNetLvlPackage aRequest aSignature    ->
                processing aChan md aSignature aTraceRouting aRequest

isItRequestForMe :: ManagerNodeData -> TraceRouting -> Bool
isItRequestForMe aData = \case
    ToNode aNodeId _      -> toNodeId (aData^.myNodeId) == aNodeId
    ToDirect _ _ _ -> False


makeRequest
    ::  TraceRouting
    ->  RequestPackage
    ->  Node.Data.Data.StringKey
    ->  CryptoFailable Package
makeRequest aTraceRouting aRequest = makeCipheredPackage
    (PackageTraceRoutingRequest aTraceRouting aRequest)


addToTrace
    ::  TraceRouting
    ->  RequestPackage
    ->  MyNodeId
    ->  ECDSA.PrivateKey
    ->  IO TraceRouting
addToTrace aTraceRouting aRequestPackage aMyNodeId aPrivateKey = do
    aTime     <- getTime Realtime
    aSignature <- signEncodeble aPrivateKey
        (aTraceRouting, aRequestPackage, aMyNodeId, aTime)
    let aPackageSignature = PackageSignature aMyNodeId aTime aSignature
    case aTraceRouting of
        ToDirect aPointFrom aPointTo aSignatures ->
            return $ ToDirect aPointFrom aPointTo (aPackageSignature : aSignatures)
        _ -> error "Node.Node.Mining.addToTrace: It is not ToDirect!"



answerToNewTransaction :: IORef ManagerNodeData -> ManagerMiningMsgBase -> IO ()
answerToNewTransaction aMd (NewTransaction aTransaction) = do
    aData <- readIORef aMd
    writeLog aData $ "I create a transaction: " ++ show aTransaction
    sendBroadcast aMd (BroadcastMining $ BroadcastTransaction aTransaction Nothing)

    writeMetric aData $ increment "net.tx.count"
    writeMetric aData $ add
        ("net.node." ++ show (toInteger $ aData^.myNodeId) ++ ".pending.amount")
        (1 :: Integer)

    writeChan (aData^.transactions) aTransaction


answerToNewTransaction _ _ = error
    "answerToNewTransaction: something unexpected  has happened."

sendBroadcast :: IORef ManagerNodeData -> BroadcastThing -> IO ()
sendBroadcast aMd aBroadcastThing = do
    aData <- readIORef aMd
    addInIndex aBroadcastThing aMd
    aPackageSignature <- makePackageSignature aData aBroadcastThing
    sendBroadcastThingToNodes aMd aPackageSignature aBroadcastThing


answerToBlockMadeMsg :: ManagerMiningMsg msg =>
    IORef ManagerNodeData -> msg -> IO ()
answerToBlockMadeMsg aMd (toManagerMiningMsg -> BlockMadeMsg aMicroblock) = do
    aData <- readIORef aMd
    writeMetric aData $ increment "net.bl.count"
    writeLog aData $ "I create a a microblock: " ++ show aMicroblock
    sendBroadcast aMd (BroadcastMining $ BroadcastMicroBlock aMicroblock Nothing)
    sendToShardingLvl aData $
        T.ShardAcceptAction (microblockToShard aMicroblock)

answerToBlockMadeMsg _ _ = pure ()


notInIndex :: Serialize a => ManagerNodeData -> a -> Bool
notInIndex aData a = not $ BI.memberR (cryptoHash a) $ aData^.hashMap


addInIndex :: Serialize a => a -> IORef ManagerNodeData -> IO ()
addInIndex aMsg aMd = do
    aTime <- getTime Realtime
    modifyIORef aMd $ hashMap %~ BI.insert aTime (cryptoHash aMsg)


processingOfBroadcastThing :: IORef ManagerNodeData -> BroadcastThing -> IO ()
processingOfBroadcastThing aMd aBroadcastThing = do
    aData <- readIORef aMd
    writeLog aData $ "Recived " ++ show aBroadcastThing
    case aBroadcastThing of
        BroadcastNet    aMsg -> processingOfBroadcast aMd aMsg
        BroadcastLogic  aMsg -> processingOfBroadcast aMd aMsg
        BroadcastMining aMsg -> processingOfBroadcast aMd aMsg

--------------------------------------------------------------------------------
