{-# LANGUAGE
        ViewPatterns
    ,   LambdaCase
    ,   MultiParamTypeClasses
    ,   FlexibleInstances
#-}
module Boot.Boot where

import qualified    Data.Map                        as M
import qualified    Data.Set                        as S
import qualified    Boot.Map.Random                 as RM
import              Data.List
import              Data.IORef
import              Control.Monad.Extra
import              Lens.Micro
import              Lens.Micro.Mtl
import              Control.Concurrent.Chan
import              Debug.Trace

import              Boot.Types
import              Node.Node.Base
import              Node.Node.Types
import              Service.Monad.Option
import              Node.Crypto
import              Node.Data.Data
import              Service.Timer

import              Node.Node.Processing
import              Node.Data.NodeTypes
import              Node.Data.NetPackage
import              Node.Data.NetMesseges

managerBootNode :: Chan ManagerBootNodeMsgBase -> IORef NodeBootNodeData -> IO ()
managerBootNode ch md = forever $ do
    mData <- readIORef md
    msg <- readChan ch
    runOption msg $ do
        baseNodeOpts ch md mData

        opt isClientIsDisconnected $ bootNodeAnswerClientIsDisconnected md

        opt isInitDatagram        $ answerToInitDatagram md
        opt isDatagramMsg         $ answerToDatagramMsg ch md (mData^.myNodeId)
        opt isCheckBroadcastNodes $ answerToCheckBroadcastNodes md ch
        opt isCheckBroadcastNode  $ answerToCheckBroadcastNode ch md


instance PackageTraceRoutingAction NodeBootNodeData RequestPackage where
    makeAction aChan md aNodeId aTraceRouting aRequesPackage = do
        aData <- readIORef md
        case aRequesPackage of
            RequestNetLvlPackage aReques aSignature ->
                processing aChan md aSignature aTraceRouting aReques
            _   -> return ()


instance  Processing (IORef NodeBootNodeData) (Request NetLvl) where
    processing aChan aMd aSignature@(PackageSignature (toNodeId -> aNodeId) _ _) aTraceRouting = \case
        BroadcastListRequest -> do
            aData <- readIORef aMd
            let aSendNetLvlResponse = sendNetLvlResponse
                    aTraceRouting aData BroadcastListRequest aSignature
            NodeInfoListNetLvl aBroadcasts <- readRecordsFromNodeListFile $ aData^.myNodeId
            let aBroadcastListResponce = BroadcastListResponce
                    (NodeInfoListLogicLvl [])
                    (NodeInfoListNetLvl $ take 10 aBroadcasts)
            aSendNetLvlResponse aBroadcastListResponce
        _ -> return ()


instance  PackageTraceRoutingAction NodeBootNodeData ResponcePackage where
    makeAction _ _ _ _ _ = return ()

instance  BroadcastAction NodeBootNodeData where
    makeBroadcastAction _ _ _ _ _ = return ()


answerToCheckBroadcastNodes
    ::  IORef NodeBootNodeData
    ->  Chan ManagerBootNodeMsgBase
    ->  ManagerBootNodeMsgBase
    ->  IO ()
answerToCheckBroadcastNodes aMd aChan _ = do
    aData <- readIORef aMd
    let
        -- активные ноды.
        aNodeIds :: [NodeId]
        aNodeIds = do
            (aId, aNode) <- M.toList $ aData^.nodes
            guard $ aNode^.status == Active
            pure aId

        (aBroadcastNodes, aNeededInBroadcastLis) = partition
            (\aId -> S.notMember aId $ aData^.checSet) aNodeIds

    forM_ aNeededInBroadcastLis $ \aNodeId -> do
        whenJust (aData^.nodes.at aNodeId) $ \aNode -> do
            sendExitMsgToNode aNode
            writeChan aChan $ checkBroadcastNode
                aNodeId (aNode^.nodeHost) (aNode^.nodePort)

    forM_ aBroadcastNodes $ \aNodeId -> do
        modifyIORef aMd $ checSet %~ S.delete aNodeId

        whenJust (aData^.nodes.at aNodeId) $ \aNode -> do
            sendExitMsgToNode aNode
            addRecordsToNodeListFile
                (aData^.myNodeId)
                (NodeInfoListNetLvl [
                    (aNodeId, aNode^.nodeHost, aNode^.nodePort)])

answerToCheckBroadcastNode :: ManagerMsg a =>
    Chan a -> IORef NodeBootNodeData -> ManagerBootNodeMsgBase -> IO ()
answerToCheckBroadcastNode aChan aMd (CheckBroadcastNode aNodeId aIp aPort) = do
    modifyIORef aMd $ checSet %~ S.insert aNodeId
    writeChan aChan $ sendInitDatagram aIp aPort aNodeId
answerToCheckBroadcastNode _ _ _ = return ()


bootNodeAnswerClientIsDisconnected ::
    IORef NodeBootNodeData -> ManagerBootNodeMsgBase -> IO ()
bootNodeAnswerClientIsDisconnected aMd
    (toManagerMsg -> ClientIsDisconnected aId aChan) = do
        traceMarkerIO "bootNodeAnswerClientIsDisconnected"
        aData <- readIORef aMd
        whenJust (aId `M.lookup` (aData^.nodes)) $ \aNode -> do
            when (aNode^.chan == aChan) $ do
                minusStatusNumber aMd aId
                modifyIORef aMd (nodes %~ M.delete aId)
bootNodeAnswerClientIsDisconnected _ _ = pure ()
