{-# LANGUAGE GADTs, DeriveGeneric, TemplateHaskell #-}

module Boot.Types where

import              Data.Monoid
import              Lens.Micro
import              System.Clock
import              Network.Socket
import              Control.Concurrent.Chan
import              Node.Template.Constructor
import qualified    Data.Set                        as S
import qualified    Data.ByteString                 as B
import qualified    Boot.Map.Random                 as RM
import              Node.Node.Types
import              Sharding.Types.Node as N

import              Node.Data.NodeTypes

data NodeBootNodeData where
    NodeBootNodeData :: {
        nodeBootNodeDataNodeConfig      :: NodeConfig,
        nodeBootNodeDataNodeBaseData    :: NodeBaseData,
        nodeBootNodeDataBroadcastNodes  :: RM.RandomMap IpPort,
        nodeBootNodeDataChecSet         :: S.Set NodeId
  } -> NodeBootNodeData


dataConstruct "ManagerBootNodeMsgBase" $
    ((_1 .~ False) <$> managerMsgFuncListData) <>
    managerBootNodeMsgListData

msgClass ["ManagerMsg"] "ManagerBootNodeMsg" managerBootNodeMsgListFull

baseMsgInstance "ManagerBootNodeMsg" "ManagerBootNodeMsgBase" managerBootNodeMsgList
derivativeMsgInstance "ManagerMsg" "ManagerBootNodeMsgBase" managerMsgFuncList


broadcastNodes :: Lens' NodeBootNodeData (RM.RandomMap (HostAddress, PortNumber))
broadcastNodes = lens nodeBootNodeDataBroadcastNodes
    (\md a -> md {nodeBootNodeDataBroadcastNodes = a})


mapM (uncurry makeLensInstance') [
    ("nodeConfig", "nodeBootNodeData"),
    ("nodeBaseData", "nodeBootNodeData")]

instance ManagerData NodeBootNodeData

instance ToManagerData NodeBootNodeData where
    toManagerData _ aMicroblockChan aExitChan aAnswerChan _ aList aNodeConfig port = NodeBootNodeData
        aNodeConfig (makeNodeBaseData aExitChan aList aAnswerChan aMicroblockChan port)
            RM.empty S.empty

lensInst "checSet" ["NodeBootNodeData"] ["S.Set", "NodeId"]
    "nodeBootNodeDataChecSet"
