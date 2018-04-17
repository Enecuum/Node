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
import              Data.IORef
import              Data.Serialize
import              Lens.Micro
import              Control.Concurrent
import              Control.Monad.Extra

import              Service.Types
import              Service.InfoMsg
import              Node.Node.Types
import              Node.Node.Base
import              Node.Data.NodeTypes
import              Node.Data.NetPackage

import qualified    Sharding.Types.Node as T
import              Node.Node.Processing
import              Sharding.Types.ShardTypes
import              Node.Data.GlobalLoging

-- обработка полученных по бродкасту сообщений, изменение своего внутреннего состояния
-- на их основе.
class BroadcastProcessing aNodeData aPackage where
    processingOfBroadcast :: aNodeData -> aPackage -> IO ()


instance BroadcastProcessing (IORef ManagerNodeData) (BroadcastThingLvl NetLvl) where
    processingOfBroadcast aMd aMsg = do
        aData <- readIORef aMd
        case aMsg of
            -- обработка события, что кто-то хочет соседей.
            INeedNeighbors (toNodeId -> aNodeId) aHostAddress aPortNumber -> do
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info $ "Accepted msg from the " ++
                    show aNodeId ++ "that it need a neighbors. Addtition the node in the list of possible connects."
                addRecordsToNodeListFile
                    (aData^.myNodeId)
                    (NodeInfoListNetLvl [(aNodeId, aHostAddress, aPortNumber)])

instance BroadcastProcessing (IORef ManagerNodeData) (BroadcastThingLvl LogicLvl) where
    processingOfBroadcast aMd aMsg = do
        aData <- readIORef aMd
        case aMsg of
            -- FIXME: переписать show для shard, показывать только хеш.
            -- обработка полученой через бродкаст шарды
            BroadcastShard aShard -> do
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info $ "Accepted new shard from broadcast. The shard is " ++ show aShard
                whenJust (aData^.shardingChan) $ \aChan ->
                    writeChan aChan $ T.NewShardInNetAction aShard

            -- обработка полученных данных, что у какой-то ноды позиция изменилась.
            BroadcastPosition     aMyNodeId aNodePosition  -> do
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info $ "Accepted new position for the node." ++
                    "The node have position " ++ show aNodePosition ++ ", node id is " ++ show aMyNodeId
                updateFile aMyNodeId
                    (NodeInfoListLogicLvl [(toNodeId aMyNodeId, aNodePosition)])
                sendToShardingLvl aData $
                    T.TheNodeHaveNewCoordinates (toNodeId aMyNodeId) aNodePosition

-- обработка (куда положить) сообщений для шардингового уровня полученная через бродкаст
instance BroadcastProcessing (IORef ManagerNodeData) (BroadcastThingLvl MiningLvl) where
    processingOfBroadcast aMd aMsg = do
        aData <- readIORef aMd
        case aMsg of
            -- добавлям полученую по сети транзакцию в пендинг
            BroadcastTransaction aTransaction _ -> do
                writeChan (aData^.transactions) aTransaction

                -- логирование и метрика
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
                    "Addtition the transaction to pending. The transaction = "
                        ++ show aTransaction
                writeMetric (aData^.infoMsgChan) $ add
                    ("net.node." ++ idShow (aData^.myNodeId) ++ ".pending.amount")
                    (1 :: Integer)

            -- добавляем микроблок в шардинг
            BroadcastMicroBlock aMicroblock _ -> do
                sendToShardingLvl aData $
                    T.ShardAcceptAction (microblockToShard aMicroblock)

                -- FIXME: переписать show для Transaction и Microblock
                --        так, чтобы они выводили только хеш и структуру.
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
                    "Addtition the mickroblock to shard DB. The mickroblock = "
                    ++ show aMicroblock
            _ -> return ()

idShow myNodeId = show (toInteger myNodeId)


-- | Преобразуем микроблок в шарду, по хорошему нужно бы ещё проверять, что её
--   хешь совпадает с тем, что нужно.
--  TODO: проверка того, что хеш блока соответсвует блоку. Подумать, когда лучше проверять.
microblockToShard :: Microblock -> Shard
microblockToShard aMicroblock@(Microblock aHash _ _) =
    Shard ShardType (Hash aHash) (encode aMicroblock)

--------------------------------------------------------------------------------
