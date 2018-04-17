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
module Node.Node.BroadcastProcessing where

--
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

import              Service.Types
import              Service.InfoMsg
import              Service.Monad.Option
import              Node.Crypto
import              Node.Data.Data
import              Node.Node.Types
import              Node.Node.Base
import              Node.Data.NodeTypes
import              Node.Data.NetPackage

import              Node.Data.NetMessages

import qualified    Sharding.Types.Node as T
import              Sharding.Space.Point
import              Node.Node.Processing
import              Lens.Micro.GHC
import              Sharding.Types.ShardTypes

--
class BroadcastProcessing aNodeData aPackage where
    processingOfBroadcast :: aNodeData -> aPackage -> IO ()

instance BroadcastProcessing (IORef ManagerNodeData) (BroadcastThingLvl NetLvl) where
    processingOfBroadcast aMd aMsg = do
        aData <- readIORef aMd
        case aMsg of
            INeedNeighbors (toNodeId -> aNodeId) aHostAddress aPortNumber ->
                addRecordsToNodeListFile
                    (aData^.myNodeId)
                    (NodeInfoListNetLvl [(aNodeId, aHostAddress, aPortNumber)])

instance BroadcastProcessing (IORef ManagerNodeData) (BroadcastThingLvl LogicLvl) where
    processingOfBroadcast aMd aMsg = do
        aData <- readIORef aMd
        case aMsg of
            BroadcastShard aShard -> do
                whenJust (aData^.shardingChan) $ \aChan ->
                    writeChan aChan $ T.NewShardInNetAction aShard

            BroadcastPosition     aMyNodeId aNodePosition  -> do
                updateFile aMyNodeId
                    (NodeInfoListLogicLvl [(toNodeId aMyNodeId, aNodePosition)])
                sendToShardingLvl aData $
                    T.TheNodeHaveNewCoordinates (toNodeId aMyNodeId) aNodePosition

-- TODO: Сделать нормальный ввод данных в метрики.
instance BroadcastProcessing (IORef ManagerNodeData) (BroadcastThingLvl MiningLvl) where
    processingOfBroadcast aMd aMsg = do
        aData <- readIORef aMd
        case aMsg of
            BroadcastTransaction aTransaction _ -> do
                writeChan (aData^.transactions) aTransaction
                writeChan (aData^.infoMsgChan) $
                    Metric $ add
                        ("net.node." ++ idShow (aData^.myNodeId) ++ ".pending.amount")
                        (1 :: Integer)
            BroadcastMicroBlock aMicroblock _ -> sendToShardingLvl aData $
                T.ShardAcceptAction (microblockToShard aMicroblock)

            _ -> return ()

idShow myNodeId = show (toInteger myNodeId)

microblockToShard :: Microblock -> Shard
microblockToShard aMicroblock@(Microblock aHash _ _) =
    Shard ShardType (Hash aHash) (encode aMicroblock)

-- data Microblock = Microblock ByteString ByteString [Transaction] deriving (Eq, Generic, Ord)
{-
BroadcastTransaction
    ::  Transaction
    ->  Maybe NodeId
    ->  BroadcastThingLvl MiningLvl

BroadcastMicroBlock
    ::  Microblock
    ->  Maybe NodeId
    ->  BroadcastThingLvl MiningLvl

BroadcastBlockIndex
    ::  B.ByteString
    ->  Maybe NodeId
    ->  BroadcastThingLvl MiningLvl

BroadcastKeyBlock
    ::  B.ByteString
    ->  Maybe NodeId
    ->  BroadcastThingLvl MiningLvl

-}

--------------------------------------------------------------------------------
