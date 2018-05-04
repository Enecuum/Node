{-# LANGUAGE GADTs, DeriveGeneric, TemplateHaskell, LambdaCase, ViewPatterns, FlexibleInstances, MultiParamTypeClasses #-}

module Boot.Types where

import              Data.Monoid
import              Lens.Micro
import              System.Clock
import              Network.Socket
import              Control.Concurrent.Chan
import              Node.Template.Constructor
import qualified    Data.Set                        as S
import qualified    Data.ByteString                 as B
import              PoA.Types
import              Node.Node.Types
import              Sharding.Types.Node as N
import              Data.IORef
import              Node.Node.Base
import              Node.Data.MakeAndSendTraceRouting

import              Node.Node.Processing
import              Node.Data.NodeTypes
import              Node.Data.NetPackage
import              Node.Data.GlobalLoging
import              Service.InfoMsg

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
    toManagerData _ aMicroblockChan aExitChan aAnswerChan aInfoChan aList aNodeConfig aPort = NodeBootNodeData
        aNodeConfig (makeNodeBaseData aExitChan aList aAnswerChan aMicroblockChan aPort aInfoChan)
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
            NodeInfoListNetLvl aBroadcasts <- readRecordsFromNodeListFile
            let aBroadcastListResponce = BroadcastListResponce
                    (NodeInfoListLogicLvl [])
                    (NodeInfoListNetLvl $ take 10 aBroadcasts)
            writeLog (aData^.infoMsgChan) [BootNodeTag, NetLvlTag] Info $
                "Send to node " ++ show aNodeId ++ " broadcast list responce " ++
                show (take 10 aBroadcasts) ++ "."
            aSendNetLvlResponse aBroadcastListResponce
        _ -> return ()


instance  PackageTraceRoutingAction NodeBootNodeData ResponcePackage where
    makeAction _ _ _ _ _ = return ()

instance  BroadcastAction NodeBootNodeData where
    makeBroadcastAction _ _ _ _ _ = return ()
