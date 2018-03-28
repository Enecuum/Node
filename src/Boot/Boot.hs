{-# LANGUAGE ViewPatterns, LambdaCase #-}
module Boot.Boot where

import qualified    Data.Map                        as M
import qualified    Data.Set                        as S
import qualified    Boot.Map.Random                 as RM
import              Data.IORef
import              Control.Monad.Extra
import              Lens.Micro
import              Control.Concurrent.Chan
import              Debug.Trace

import              Boot.Types
import              Node.Node.Base
import              Node.Node.Types
import              Service.Monad.Option
import              Node.Crypto
import              Node.Data.Data
import              Service.Timer

import              Node.Data.NodeTypes
import              Node.Data.NetPackage
import              Node.Data.NetMesseges

managerBootNode :: Chan ManagerBootNodeMsgBase -> IORef NodeBootNodeData -> IO ()
managerBootNode ch md = forever $ do
    mData <- readIORef md
    msg <- readChan ch
    --debug (mData^.outChan) $ "manager " <> show msg
    runOption msg $ do
        baseNodeOpts ch md mData

        opt isClientIsDisconnected $ bootNodeAnswerClientIsDisconnected md

        opt isInitDatagram      $ answerToInitDatagram md
        opt isDatagramMsg       $ answerToDatagramMsg ch md (mData^.myNodeId)
            bootNodeAnswerToPing
            (answerToPong
                :: PongAnswer NodeBootNodeData ManagerBootNodeMsgBase)
            bootNodeAnswerToInfoPing
        opt isCheckBroadcastNodes $ answerToCheckBroadcastNodes md ch
        opt isCheckBroadcastNode  $ answerToCheckBroadcastNode ch md


answerToCheckBroadcastNodes ::
    IORef NodeBootNodeData ->
    Chan ManagerBootNodeMsgBase ->
    ManagerBootNodeMsgBase -> IO ()
answerToCheckBroadcastNodes aMd aChan _ = do
    aData <- readIORef aMd
    let anActiveNodes :: [Node]
        anActiveNodes = getNodes Active aData

        aNodeIds :: [NodeId]
        aNodeIds = (\aNode -> keyToId $ (aNode^.nPublicKey)) <$> anActiveNodes

        aNeededInBroadcastLis :: [NodeId]
        aNeededInBroadcastLis = filter (\aId -> S.notMember aId $ aData^.checSet)
            aNodeIds

        aBroadcastNodes :: [NodeId]
        aBroadcastNodes = filter (\aId -> S.member aId $ aData^.checSet)
            aNodeIds

    forM_ aNeededInBroadcastLis $ \aNodeId -> do
        aBroadcastNodeList <- aData^.broadcastNodes.to (RM.takeRandom 10)
        sendJustPackagedMsg $ makeMsg aNodeId aData $
            makePingPongMsg Pong $ BroadcastNodeListAnswer aBroadcastNodeList
        whenJust (aNodeId `M.lookup` (aData^.nodes)) $ \aNode -> do
            timer 100000 $ do
                sendExitMsgToNode aNode
                whenJust (aGetIpAndPort aData aNodeId) $ \(aIp, aPort) -> do
                    timer 100000 $ do
                        writeChan aChan $ checkBroadcastNode aNodeId aIp aPort

    forM_ aBroadcastNodes $ \aNodeId -> do
        modifyIORef aMd $ checSet %~ S.delete aNodeId

        whenJust (aNodeId `M.lookup` (aData^.nodes)) sendExitMsgToNode
        whenJust (aGetIpAndPort aData aNodeId) $ \(aIp, aPort) -> do
            modifyIORef aMd $ broadcastNodes %~ RM.insert aNodeId (aIp, aPort)
  where
    aGetIpAndPort aData aNodeId = do
        aNode     <- aNodeId `M.lookup` (aData^.nodes)
        aHelloMsg <- aNode^.mHelloMsg
        return (aNode^.nHostAddress, aHelloMsg^.listenPort)


answerToCheckBroadcastNode :: ManagerMsg a =>
    Chan a -> IORef NodeBootNodeData -> ManagerBootNodeMsgBase -> IO ()
answerToCheckBroadcastNode aChan aMd (CheckBroadcastNode aNodeId aIp aPort) = do
    modifyIORef aMd $ checSet %~ S.insert aNodeId
    sendInitDatagramFunc aChan aIp aPort aNodeId aMd
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


bootNodeAnswerToPing :: PingAnswer NodeBootNodeData ManagerBootNodeMsgBase
bootNodeAnswerToPing _ _ _ _ = pure ()


bootNodeAnswerToPong :: PongAnswer NodeBootNodeData ManagerBootNodeMsgBase
bootNodeAnswerToPong _ _ _ _ = pure ()


bootNodeAnswerToInfoPing :: InfoPingAnswer  NodeBootNodeData ManagerBootNodeMsgBase
bootNodeAnswerToInfoPing _ _ _ _ = pure ()
