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

import qualified    Data.ByteString                 as B
import qualified    Data.Map                        as M
import qualified    Data.Bimap                      as BI
import              System.Clock
import              Data.IORef
import              Data.Serialize
import              Lens.Micro
import              Control.Concurrent
import              Control.Monad.Extra
import              Crypto.Error
import              Node.Node.BroadcastProcessing

import              Service.Monad.Option
import              Node.Crypto
import              Node.Data.Data
import              Node.Node.Types
import              Node.Node.Base
import              Node.Data.NodeTypes
import              Node.Data.NetPackage
import              Node.Data.NetMesseges
import qualified    Sharding.Types.Node as T
import              Sharding.Space.Point
import              Node.Node.Processing
import              Lens.Micro.GHC


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


miningNodeAnswerClientIsDisconnected
    ::  IORef ManagerNodeData
    ->  ManagerMiningMsgBase
    ->  IO ()
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
answerToDeleteOldestVacantPositions
    ::  IORef ManagerNodeData
    ->  ManagerMiningMsgBase
    ->  IO ()
answerToDeleteOldestVacantPositions aMd _ = do
    aTime <- getTime Realtime
    modifyIORef aMd $ vacantPositions %~ BI.filter (\aTimeSpec _ ->
        diffTimeSpec aTime aTimeSpec > 3000000)

----TODO: MOVE TO ?????? --------------
answerToDeleteOldestMsg
    ::  IORef ManagerNodeData
    ->  ManagerMiningMsgBase
    ->  IO ()
answerToDeleteOldestMsg aMd _ = do
    aTime <- getTime Realtime
    modifyIORef aMd $ hashMap %~ deleteOldest aTime


instance BroadcastAction ManagerNodeData where
    makeBroadcastAction _ aMd _ aBroadcastSignature aBroadcastThing = do
        aData <- readIORef aMd
        loging aData $ "BroadcastAction ManagerNodeData" ++ show aBroadcastThing
        when (notInIndex aData aBroadcastThing) $ do
            addInIndex aBroadcastThing aMd
            sendBroadcastThingToNodes aMd aBroadcastSignature aBroadcastThing
            processingOfBroadcastThing aMd aBroadcastThing

{-
TODO verification
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
    makeAction aChan md aNodeId aTraceRouting aResponcePackage = do
        aData <- readIORef md
        when (verifyResponce aTraceRouting aResponcePackage) $ if
            | isItMyResponce aNodeId aTraceRouting  -> aProcessingOfAction
            | otherwise                             -> aSendToNeighbor aData
      where
        verifyResponce _ _ = True -- TODO : add body
        aProcessingOfAction = case aResponcePackage of
            ResponceNetLvlPackage _ aResponse aSignature   | True ->
                processing aChan md aSignature aTraceRouting aResponse
            ResponceLogicLvlPackage _ aResponse aSignature | True ->
                processing aChan md aSignature aTraceRouting aResponse

        aSendToNeighbor aData = do
            let (aNode, aNewTrace) = getClosedNode aTraceRouting aData
                aMaybePoints = case aTraceRouting of
                    ToDirect aPointFrom aPointTo _
                        -> Just (aPointFrom, aPointTo)
                    _   -> Nothing
            whenJust aMaybePoints $ \(aPointFrom, aPointTo) ->
                whenJust aNode $ sendToNode (makeResponse
                    (ToDirect aPointFrom aPointTo aNewTrace) aResponcePackage)


isItMyResponce :: NodeId -> TraceRouting -> Bool
isItMyResponce aMyNodeId = \case
    ToNode   _ (PackageSignature aNodeId _ _)
        | toNodeId aNodeId == aMyNodeId -> True
    ToDirect _ _ (last -> (PackageSignature (toNodeId -> aNodeId) _ _))
        | aNodeId == aMyNodeId          -> True
    _                                   -> False


---------------TODO: fix True---------------------------------------------------
instance PackageTraceRoutingAction ManagerNodeData RequestPackage where
    makeAction aChan md _ aTraceRouting aRequestPackage = do
        aData <- readIORef md
        when True $ if
            | True                                 -> aProcessingOfAction
            | otherwise                            -> aSendToNeighbor aData
      where
        aProcessingOfAction = case aRequestPackage of
            RequestLogicLvlPackage aRequest aSignature
                | True -> processing aChan md aSignature aTraceRouting aRequest
            RequestNetLvlPackage aRequest aSignature
                | True -> processing aChan md aSignature aTraceRouting aRequest

        aSendToNeighbor aData = case aTraceRouting of
            ToDirect _ aPointTo _ ->
                whenJust (getClosedNodeByDirect aData (toPoint aPointTo)) $
                    \aNode -> do
                        aNewTrace <- addToTrace
                            aTraceRouting
                            aRequestPackage
                            (aData^.myNodeId)
                            (aData^.privateKey)
                        sendToNode (makeRequest aNewTrace aRequestPackage) aNode
            _ -> return ()

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
    --metric $ increment "net.tx.count"
    aData <- readIORef aMd
    loging aData $ "I create a transaction: " ++ show aTransaction
    let aBroadcastThing = BroadcastMining $
            BroadcastTransaction aTransaction Nothing
    addInIndex aBroadcastThing aMd

    aPackageSignature <- makePackageSignature aData aBroadcastThing
    sendBroadcastThingToNodes aMd aPackageSignature aBroadcastThing
    {-
    metric $ add
        ("net.node." ++ show (toInteger $ aData^.myNodeId) ++ ".pending.amount")
        (1 :: Integer)
    -}
    writeChan (aData^.transactions) aTransaction

answerToNewTransaction _ _ = error
    "answerToNewTransaction: something unexpected  has happened."


answerToBlockMadeMsg :: ManagerMiningMsg msg =>
    IORef ManagerNodeData -> msg -> IO ()
answerToBlockMadeMsg aMd (toManagerMiningMsg -> BlockMadeMsg aMicroblock) = do
    --metric $ increment "net.bl.count"
    aData <- readIORef aMd
    loging aData $ "I create a a microblock: " ++ show aMicroblock
    let aBroadcastThing = BroadcastMining $
            BroadcastMicroBlock aMicroblock Nothing
    addInIndex aBroadcastThing aMd

    aPackageSignature <- makePackageSignature aData aBroadcastThing
    sendBroadcastThingToNodes aMd aPackageSignature aBroadcastThing

--    writeChan (aData^.microblockChan) aMicroblock
answerToBlockMadeMsg _ _ = pure ()


notInIndex :: Serialize a => ManagerNodeData -> a -> Bool
notInIndex aData a = not $ BI.memberR (cryptoHash a) $ aData^.hashMap


addInIndex :: Serialize a => a -> IORef ManagerNodeData -> IO ()
addInIndex aMsg aMd = do
    aTime <- getTime Realtime
    modifyIORef aMd $ hashMap %~ BI.insert aTime (cryptoHash aMsg)

deleteOldest
    ::  TimeSpec
    ->  BI.Bimap TimeSpec B.ByteString
    ->  BI.Bimap TimeSpec B.ByteString
deleteOldest aTime = BI.filter
    (\aOldTime _ -> diffTimeSpec aOldTime aTime < fromNanoSecs 3000000)


isBootNode :: NodeId -> ManagerNodeData -> Bool
isBootNode aId aData = aId `elem` ((^._1) <$> aData^.nodeBaseData.bootNodes)


eq :: MyNodeId -> NodeId -> Bool
eq (MyNodeId aMyNodeId) (NodeId aNodeId) = aMyNodeId == aNodeId


processingOfBroadcastThing :: IORef ManagerNodeData -> BroadcastThing -> IO ()
processingOfBroadcastThing aMd aBroadcastThing = do
    aData <- readIORef aMd
    loging aData $ "Recived " ++ show aBroadcastThing
    case aBroadcastThing of
        BroadcastNet    aMsg -> processingOfBroadcast aMd aMsg
        BroadcastLogic  aMsg -> processingOfBroadcast aMd aMsg
        BroadcastMining aMsg -> processingOfBroadcast aMd aMsg


--------------------------------------------------------------------------------
