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
module Node.Node.Mining (
    managerMining
  ) where

import              System.Random()
import qualified    Crypto.PubKey.ECC.ECDSA         as ECDSA

import qualified    Data.Map                        as M
import qualified    Data.Bimap                      as BI
import              Data.Maybe (isNothing)
import              Data.List.Extra
import              System.Clock
import              Data.IORef
import              Data.Serialize
import              Lens.Micro
import              Lens.Micro.Mtl()
import              Control.Concurrent
import              Control.Monad.Extra
import              Crypto.Error
import              Node.Node.BroadcastProcessing

import              Node.FileDB.FileServer
import              Sharding.Space.Distance
import              Sharding.Types.ShardLogic
import              Service.Monad.Option
import              Node.Crypto
import              Node.Data.Key
import              Node.Node.Types
import              Node.Node.Base
import              Node.Data.NetPackage
import qualified    Sharding.Types.Node as T
import              Sharding.Space.Point
import              Node.Node.Processing
import              Service.InfoMsg
import              Node.Data.MakeAndSendTraceRouting
import              Node.Data.Verification
import              Node.Data.GlobalLoging
import              PoA.Types
import              Sharding.Sharding()
import              Node.BaseFunctions

managerMining :: Chan ManagerMiningMsgBase -> IORef ManagerNodeData -> IO ()
managerMining aChan aMd = do
  modifyIORef aMd $ iAmBroadcast .~ True -- FIXME:!!!
  aData <- readIORef aMd
  undead (writeLog (aData^.infoMsgChan) [NetLvlTag] Warning $
      "managerMining. This node could be die!" ) 
      $ forever $ do
          mData <- readIORef aMd
          readChan aChan >>= \a -> runOption a $ do
            baseNodeOpts aChan aMd mData

            opt isInitDatagram          $ answerToInitDatagram aMd
            opt isDatagramMsg           $ answerToDatagramMsg aChan aMd (mData^.myNodeId)
            opt isClientIsDisconnected $ miningNodeAnswerClientIsDisconnected aMd

            opt isTestBroadcastBlockIndex   $ answerToTestBroadcastBlockIndex aMd
            opt isNewTransaction            $ answerToNewTransaction aMd
            opt isInitDatagram              $ answerToSendInitDatagram aChan aMd
            opt isShardingNodeRequestMsg    $ answerToShardingNodeRequestMsg aMd
            opt isDeleteOldestMsg           $ answerToDeleteOldestMsg aMd
            opt isDeleteOldestPoW           $ answerToDeleteOldestPoW aMd
            opt isMsgFromPP                 $ answeToMsgFromPP aMd
            opt isInitShardingLvl           $ answerToInitShardingLvl aChan aMd
            opt isInfoRequest               $ answerToInfoRequest aMd


answerToInfoRequest :: ManagerData md => IORef md -> a -> IO ()
answerToInfoRequest aMd _ = do
    aData <- readIORef aMd
    let aIds = M.keys $ aData^.nodes
    writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
        "answerToInfoRequest: Node broadcast list request to " ++ show (length aIds) ++ " nodes."
    makeAndSendTo aData aIds BroadcastListRequest


answerToInitShardingLvl :: (ManagerMsg msg, NodeConfigClass s, NodeBaseDataClass s) =>
                            Chan msg -> IORef s -> p -> IO ()
answerToInitShardingLvl aChan aMd _ = do
    aData <- readIORef aMd
    when (isNothing (aData^.myNodePosition)) $ do
        writeLog (aData^.infoMsgChan) [NetLvlTag, InitTag] Info
            "Select new random coordinate because I am first node in net."
        initShading aChan aMd


answerToTestBroadcastBlockIndex :: IORef ManagerNodeData -> ManagerMiningMsgBase ->  IO ()
answerToTestBroadcastBlockIndex aMd _ = do
    aData <- readIORef aMd
    aPosChan <- newChan
    writeChan (aData^.fileServerChan) $ FileActorRequestLogicLvl $ ReadRecordsFromNodeListFile aPosChan
    NodeInfoListLogicLvl aPossitionList <- readChan aPosChan
    writeLog (aData^.infoMsgChan) [NetLvlTag] Info $ "Point list XXX: " ++ show aPossitionList
    writeLog (aData^.infoMsgChan) [NetLvlTag] Info $ "Point node list XXX: " ++ show ((^.nodePosition) <$> M.elems (aData^.nodes))



answeToMsgFromPP :: IORef ManagerNodeData ->  ManagerMiningMsgBase ->  IO ()
answeToMsgFromPP aMd (toManagerMsg -> MsgFromPP aMsg) = do
    aData <- readIORef aMd
    writeLog (aData^.infoMsgChan) [NetLvlTag] Info $  "Recived msg from PP " ++ show aMsg
    case aMsg of
        MicroblockFromPP aMicroblock aSenderId -> do
            writeMetric (aData^.infoMsgChan)  $ increment "net.bl.count"
            writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
                "PP node " ++ show aSenderId ++ ", create a a microblock: " ++ show aMicroblock
            sendBroadcast aMd (BroadcastMicroBlock aMicroblock Nothing)
            sendToShardingLvl aData $
                T.ShardAcceptAction (microblockToShard aMicroblock)

        NewConnectWithPP aPPId aNodeType aChanNNToPPMessage  -> do
            writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
                "A new connect with PP node " ++ show aPPId ++ ", the type of node is " ++ show aNodeType
            modifyIORef aMd $ ppNodes %~ M.insert aPPId
                (PPNode aNodeType aChanNNToPPMessage)
            let aRequest = RequestPPConnection aPPId
            makeAndSendTo aData (ppIdToNodePosition aPPId) aRequest

        BroadcastRequestFromPP aByteString aIdFrom@(IdFrom aPPId) aNodeType ->
            whenJust (aData^.ppNodes.at aPPId) $ \aNode -> do
                let aMessage = BroadcastPPMsg (aNode^.ppType) aByteString aNodeType aIdFrom
                sendBroadcast aMd aMessage
                processingOfBroadcast aMd aMessage

        MsgResendingToPP aIdFrom@(IdFrom aPPIdFrom) aIdTo@(IdTo aId) aByteString
            | Just aNode <- aData^.ppNodes.at aId ->
              writeChan (aNode^.ppChan) $ MsgMsgToPP aPPIdFrom aByteString
            | otherwise -> do
                let aRequest = PPMessage aByteString aIdFrom aIdTo
                makeAndSendTo aData (ppIdToNodePosition aId) aRequest
        PoWListRequest (IdFrom aPPIdFrom) ->
            whenJust (aData^.ppNodes.at aPPIdFrom) $ \aPpNode -> do
                let aPPIds = takeEnd 5 (map snd (BI.toList $ aData^.poWNodes))
                writeChan (aPpNode^.ppChan) $ ResponsePoWList aPPIds
  where
    ppIdToNodePosition :: PPId -> NodePosition
    ppIdToNodePosition (PPId aPPId) = toNodePosition $ idToPoint aPPId



answeToMsgFromPP _ _ = error "answeToMsgFromPP"

-- TODO: определение "места" где должа находиться PP нода. (переконект)

miningNodeAnswerClientIsDisconnected
    ::  IORef ManagerNodeData
    ->  ManagerMiningMsgBase
    ->  IO ()
miningNodeAnswerClientIsDisconnected aMd
    (toManagerMsg -> ClientIsDisconnected aNodeId aChan) = do
        aData <- readIORef aMd
        whenJust (aNodeId `M.lookup`(aData^.nodes)) $ \aNode -> do
            writeLog (aData^.infoMsgChan) [NetLvlTag] Info $  "The node " ++ show aNodeId ++ " is disconnected."
            when (aChan == (aNode^.chan)) $
                modifyIORef aMd $ nodes %~ M.delete aNodeId
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
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info $aLogMsg "is this neighbor alive" ++
                    "Noda about which they asked: " ++  show aNodeId
        case aNetLvlMsg of
            T.NewPosiotionMsg aMyNodePosition -> do
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info $aLogMsg "new position"
                    ++ ". The position is "
                    ++ show aMyNodePosition ++ "."
                sendBroadcast aMd
                    (BroadcastPosition
                        (aData^.myNodeId)
                        (toNodePosition aMyNodePosition))

            T.IamAwakeRequst _ aMyNodePosition -> do
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info $aLogMsg "awake logic lvl"
                sendBroadcast aMd
                    (BroadcastPosition
                        (aData^.myNodeId)
                        (toNodePosition aMyNodePosition))

            T.NeighborListRequest -> do
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info $aLogMsg "neighbors"
                makeAndSendTo aData (M.keys $ aData^.nodes)
                    NeighborListRequestPackage

            T.ShardIndexRequest aDistance aNodePositions -> do
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info $ aLogMsg "hashes of needed shards"
                whenJust (aData^.myNodePosition) $ \aMyPosition -> do
                    let aRequest = ShardIndexRequestPackage
                            (toNodePosition aMyPosition) aDistance
                    forM_ aNodePositions $ \aPosition ->
                        makeAndSendTo aData aPosition aRequest

            T.ShardListRequest shardHashes -> do
                writeLog (aData^.infoMsgChan) [NetLvlTag, LoadingShardsTag] Info $
                    aLogMsg "needed shards"
                writeLog (aData^.infoMsgChan) [NetLvlTag, LoadingShardsTag] Info $
                    "The list of requested shards:" ++ show shardHashes

                forM_ shardHashes $ \aHash -> do
                    let aPosition = NodePosition $ hashToPoint aHash
                        aRequest  = ShardRequestPackage aHash
                    makeAndSendTo aData aPosition aRequest

            T.IsTheNeighborAliveRequest aNodeId aNodePosition
                | aData^.iAmBroadcast -> if
                    | Just _ <- aData^.nodes.at aNodeId -> do
                        aLogAboutAliveRequest aNodeId
                        writeLog (aData^.infoMsgChan) [NetLvlTag] Info "The node is alive."
                        return ()
                    | otherwise -> do
                        aLogAboutAliveRequest aNodeId
                        writeLog (aData^.infoMsgChan) [NetLvlTag] Info "The node is dead."
                        sendToShardingLvl aData $ T.TheNodeIsDead aNodeId
                | otherwise -> do
                    let aListOfBroatcastPosition = concat $ do
                            (aId, aNode) <- M.toList $ aData^.nodes
                            if  | Just aPosition <- aNode^.nodePosition
                                    -> return [(aId, aPosition)]
                                | otherwise -> return []

                        aBroadcastNodeId = take 1 $ (^._1) <$> sortOn
                            (\a -> distanceTo (a^._2) aNodePosition)
                            aListOfBroatcastPosition
                    aLogAboutAliveRequest aNodeId
                    writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
                        "Request to broadcast node about state (alive or dead) of " ++ show aNodeId
                    makeAndSendTo aData aBroadcastNodeId
                        (IsAliveTheNodeRequestPackage aNodeId)
answerToShardingNodeRequestMsg _ _ = return ()


answerToDeleteOldestMsg
    ::  IORef ManagerNodeData
    ->  ManagerMiningMsgBase
    ->  IO ()
answerToDeleteOldestMsg aMd _ = do
    aData <- readIORef aMd
    writeLog (aData^.infoMsgChan) [NetLvlTag] Info "Cleaning of index of bradcasted msg."
    aTime <- getTime Realtime
    modifyIORef aMd $ hashMap %~ BI.filter
        (\aOldTime _ -> diffTimeSpec aOldTime aTime < fromNanoSecs 3000000)

--
answerToDeleteOldestPoW
    ::  IORef ManagerNodeData
    ->  ManagerMiningMsgBase
    ->  IO ()
answerToDeleteOldestPoW aMd _ = do
    aData <- readIORef aMd
    writeLog (aData^.infoMsgChan) [NetLvlTag] Info "Cleaning of index of PoW."
    aTime <- getTime Realtime
    modifyIORef aMd $ hashMap %~ BI.filter
        (\aOldTime _ -> diffTimeSpec aOldTime aTime < fromNanoSecs timeLimit)

timeLimit :: Integer
timeLimit = 5*60*10^(9 :: Integer)

instance BroadcastAction ManagerNodeData where
    makeBroadcastAction _ aMd _ aBroadcastSignature aBroadcastThing = do
        aData <- readIORef aMd
        writeLog (aData^.infoMsgChan) [NetLvlTag] Info $ "Recived the broadcast msg " ++ show aBroadcastThing ++ "."
        when (notInIndex aData aBroadcastThing) $ do
            addInIndex aBroadcastThing aMd
            sendBroadcastThingToNodes aMd aBroadcastSignature aBroadcastThing
            processingOfBroadcastThing aMd aBroadcastThing


instance PackageTraceRoutingAction ManagerNodeData ResponsePackage where
    makeAction aChan md aNodeId aTraceRouting aResponsePackage = do
        aData <- readIORef md
        writeLog (aData^.infoMsgChan) [NetLvlTag] Info "Recived a Response package."
        if verify (aTraceRouting, aResponsePackage) then if
            | isItMyResponse aNodeId aTraceRouting  -> do
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info "The Response is for me. The processing of Response."
                aProcessingOfAction
            | otherwise -> aSendToNeighbor aData
        else writeLog (aData^.infoMsgChan) [NetLvlTag] Warning $
            "The error of verification of Response package. The trace is a "
            ++ show aTraceRouting
            ++ "the package is " ++ show aResponsePackage
      where
        aProcessingOfAction = case aResponsePackage of
            ResponseNetLvlPackage _ aResponse aSignature ->
                processing aChan md aSignature aTraceRouting aResponse
            ResponseLogicLvlPackage _ aResponse aSignature ->
                processing aChan md aSignature aTraceRouting aResponse
            ResponseMiningLvlPackage _ aResponse aSignature ->
                processing aChan md aSignature aTraceRouting aResponse

        aSendToNeighbor aData = do
            writeLog (aData^.infoMsgChan) [NetLvlTag] Info "This is someone else's message. Resending of Response."
            let (aNode, aNewTrace) = getClosedNode aTraceRouting aData
                aMaybePoints = case aTraceRouting of
                    ToDirect aPointFrom aPointTo _
                        -> Just (aPointFrom, aPointTo)
                    _   -> Nothing
            whenJust aMaybePoints $ \(aPointFrom, aPointTo) ->
                whenJust aNode $ sendToNode (makeResponse
                    (ToDirect aPointFrom aPointTo aNewTrace) aResponsePackage)


isItMyResponse :: NodeId -> TraceRouting -> Bool
isItMyResponse aMyNodeId = \case
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
                let aMaybeNode = getClosedNodeByDirect aData (toPoint aPointTo) aIsMiningLvlMsg
                whenJust aMaybeNode $ \aNode -> do
                    aNewTrace <- addToTrace
                        aTraceRouting
                        aRequestPackage
                        (aData^.myNodeId)
                        (aData^.privateKey)
                    sendToNode (makeRequest aNewTrace aRequestPackage) aNode
                when (isNothing aMaybeNode) aProcessingOfAction
            ToNode aNodeId _
                | toNodeId (aData^.myNodeId) == aNodeId -> aProcessingOfAction
                | otherwise -> writeLog (aData^.infoMsgChan) [NetLvlTag] Warning $
                    "The package is to " ++ show aNodeId ++ "but i am a " ++
                    show (aData^.myNodeId) ++ ". The package is a " ++ show aRequestPackage

      where
        aProcessingOfAction = case aRequestPackage of
            RequestLogicLvlPackage aRequest aSignature  ->
                processing aChan md aSignature aTraceRouting aRequest
            RequestNetLvlPackage aRequest aSignature    ->
                processing aChan md aSignature aTraceRouting aRequest
            RequestMiningLvlPackage aRequest aSignature ->
                processing aChan md aSignature aTraceRouting aRequest

        aIsMiningLvlMsg = case aRequestPackage of
            RequestMiningLvlPackage{}  -> True
            _                          -> False


makeRequest
    ::  TraceRouting
    ->  RequestPackage
    ->  StringKey
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
    writeLog (aData^.infoMsgChan) [NetLvlTag] Info $ "I create a transaction: " ++ show aTransaction
    sendBroadcast aMd (BroadcastTransaction aTransaction Nothing)

    writeMetric (aData^.infoMsgChan)  $ increment "net.tx.count"
    writeMetric (aData^.infoMsgChan)  $ add
        ("net.node." ++ show (toInteger $ aData^.myNodeId) ++ ".pending.amount")
        (1 :: Integer)

    writeChan (aData^.transactions) aTransaction


answerToNewTransaction _ _ = error
    "answerToNewTransaction: something unexpected  has happened."


---- IDEA: Вынести в отдельный модуль????
--------------------------------------------------------------------------------
class SendBroadcast a where
    sendBroadcast :: IORef ManagerNodeData -> a -> IO ()

instance SendBroadcast BroadcastThing where
    sendBroadcast aMd aBroadcastThing = do
        aData <- readIORef aMd
        addInIndex aBroadcastThing aMd
        aPackageSignature <- makePackageSignature aData aBroadcastThing
        sendBroadcastThingToNodes aMd aPackageSignature aBroadcastThing

instance SendBroadcast (BroadcastThingLvl NetLvl) where
    sendBroadcast aMd = sendBroadcast aMd . BroadcastNet

instance SendBroadcast (BroadcastThingLvl LogicLvl) where
    sendBroadcast aMd = sendBroadcast aMd . BroadcastLogic

instance SendBroadcast (BroadcastThingLvl MiningLvl) where
    sendBroadcast aMd = sendBroadcast aMd . BroadcastMining
--------------------------------------------------------------------------------


notInIndex :: Serialize a => ManagerNodeData -> a -> Bool
notInIndex aData a = not $ BI.memberR (cryptoHash a) $ aData^.hashMap


addInIndex :: Serialize a => a -> IORef ManagerNodeData -> IO ()
addInIndex aMsg aMd = do
    aTime <- getTime Realtime
    modifyIORef aMd $ hashMap %~ BI.insert aTime (cryptoHash aMsg)


processingOfBroadcastThing :: IORef ManagerNodeData -> BroadcastThing -> IO ()
processingOfBroadcastThing aMd aBroadcastThing = do
    aData <- readIORef aMd
    writeLog (aData^.infoMsgChan) [NetLvlTag] Info $ "Recived " ++ show aBroadcastThing
    case aBroadcastThing of
        BroadcastNet    aMsg -> processingOfBroadcast aMd aMsg
        BroadcastLogic  aMsg -> processingOfBroadcast aMd aMsg
        BroadcastMining aMsg -> processingOfBroadcast aMd aMsg

--------------------------------------------------------------------------------
