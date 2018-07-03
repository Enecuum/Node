{-# LANGUAGE
        GADTs
    ,   DeriveGeneric
    ,   TemplateHaskell
    ,   LambdaCase
    ,   ViewPatterns
    ,   FlexibleInstances
    ,   MultiParamTypeClasses
  #-}

module Boot.Types where

import              Data.Monoid
import              Lens.Micro
import              System.Clock
import              Network.Socket
import qualified    Control.Concurrent.Chan         as C
import              Node.Template.Constructor
import qualified    Data.Set                        as S
import qualified    Data.ByteString                 as B
import              Node.Node.Types
import              Control.Concurrent.Chan.Unagi.Bounded
import              Sharding.Types.Node as N
import              Data.IORef
import              Node.Node.Base
import              Control.Concurrent.MVar
import              Node.Data.MakeAndSendTraceRouting

import              Node.Node.Processing
import              Node.Data.NetPackage
import              Node.Data.GlobalLoging
import              Service.InfoMsg
import              Node.Data.Key
import              Node.FileDB.FileServer

data NodeBootNodeData where
    NodeBootNodeData :: {
        nodeBootNodeDataNodeConfig      :: NodeConfig,
        nodeBootNodeDataNodeBaseData    :: NodeBaseData,
        nodeBootNodeDataChecSet         :: S.Set NodeId
  } -> NodeBootNodeData


dataConstruct "ManagerBootNodeMsgBase" $
    ((_1 .~ False) <$> managerMsgFuncListData) <>
    managerBootNodeMsgListData

msgClass ["ManagerMsg"] "ManagerBootNodeMsg" managerBootNodeMsgListFull

baseMsgInstance "ManagerBootNodeMsg" "ManagerBootNodeMsgBase" managerBootNodeMsgList
derivativeMsgInstance "ManagerMsg" "ManagerBootNodeMsgBase" managerMsgFuncList



mapM (uncurry makeLensInstance') [
    ("nodeConfig", "nodeBootNodeData"),
    ("nodeBaseData", "nodeBootNodeData")]

instance ManagerData NodeBootNodeData

instance ToManagerData NodeBootNodeData where
    toManagerData _ aMicroblockChan aVlalueChan aExitChan aAnswerChan aInfoChan aFileChan aList aNodeConfig aPort = NodeBootNodeData
        aNodeConfig (makeNodeBaseData aExitChan aList aAnswerChan aMicroblockChan aVlalueChan aPort aInfoChan aFileChan)
            S.empty

lensInst "checSet" ["NodeBootNodeData"] ["S.Set", "NodeId"]
    "nodeBootNodeDataChecSet"



instance PackageTraceRoutingAction NodeBootNodeData RequestPackage where
    makeAction aChan aMd _ aTraceRouting aRequesPackage =
        case aRequesPackage of
            RequestNetLvlPackage aReques aSignature ->
                processing aChan aMd aSignature aTraceRouting aReques
            _   -> return ()


instance  Processing (IORef NodeBootNodeData) (Request NetLvl) where
    processing _ aMd aSignature@(PackageSignature (toNodeId -> aNodeId) _ _) aTraceRouting = \case
        BroadcastListRequest -> do
            aData <- readIORef aMd
            let aSendNetLvlResponse = sendResponseTo
                    aTraceRouting aData BroadcastListRequest aSignature

            aConChan <- newEmptyMVar
            writeChan (aData^.fileServerChan) $ FileActorRequestNetLvl $ ReadRecordsFromNodeListFile aConChan
            NodeInfoListNetLvl aBroadcasts <- takeMVar aConChan

            let aBroadcastListResponse = BroadcastListResponse
                    (NodeInfoListLogicLvl [])
                    (NodeInfoListNetLvl $ take 10 aBroadcasts)
                    True
            writeLog (aData^.infoMsgChan) [BootNodeTag, NetLvlTag] Info $
                "Send to node " ++ show aNodeId ++ " broadcast list Response " ++
                show (take 10 aBroadcasts) ++ "."
            aSendNetLvlResponse aBroadcastListResponse
        _ -> return ()


instance  PackageTraceRoutingAction NodeBootNodeData ResponsePackage where
    makeAction _ _ _ _ _ = return ()

instance  BroadcastAction NodeBootNodeData where
    makeBroadcastAction _ _ _ _ _ = return ()
