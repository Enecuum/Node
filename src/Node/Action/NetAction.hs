{-# LANGUAGE MultiParamTypeClasses #-}
module Node.Action.NetAction where

import              Node.Node.Types
import              Node.Data.NetPackage
import              Node.Data.NodeTypes

import              Data.IORef
import              System.Clock
import              Crypto.PubKey.ECC.ECDSA (Signature(..))


type PingPongAction t md r = t -> IORef md -> NodeId -> r -> IO ()
type ShardingAction t md r = t -> IORef md -> NodeId -> [(NodeId, TimeSpec, Signature)] -> r -> IO ()

class ManagerData md => NetAction md where
    actionByPing            :: PingPongAction t md PingPackage
    actionByPong            :: PingPongAction t md PongPackage
    actionByInfoPing        :: PingPongAction t md InfoPingPackage

    actionByRequest         :: ShardingAction t md RequestPackage
    actionByAnswerMsg       :: ShardingAction t md AnswerPackage
    actionByConfirmRequest  :: ShardingAction t md ConfirmationOfRequestPackage
