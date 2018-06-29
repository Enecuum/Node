{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
module Node.Node.BroadcastProcessing (
        BroadcastProcessing(..)
    ,   microblockToShard
  ) where

--
import qualified Control.Concurrent                    as C
import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Monad.Extra
import qualified Data.Aeson                            as A
import qualified Data.Bimap                            as BI
import           Data.IORef
import qualified Data.Map                              as M
import           Lens.Micro
import           Lens.Micro.Mtl                        ()
import           System.Clock

import           Node.Data.NetPackage
import           Node.Node.Types
import           PoA.Types
import           Service.InfoMsg
import           Service.Types

import           Node.Data.GlobalLoging
import           Node.Data.Key
import           Node.FileDB.FileServer
import           Node.Node.Processing
import           Service.Network.Base
import qualified Sharding.Types.Node                   as T
import           Sharding.Types.ShardTypes


-- Handling received broadcast messages and change internal state based on it.
class BroadcastProcessing aNodeData aPackage where
    processingOfBroadcast :: aNodeData -> aPackage -> IO ()


instance BroadcastProcessing (IORef ManagerNodeData) (BroadcastThingLvl NetLvl) where
    processingOfBroadcast aMd aMsg = do
        aData <- readIORef aMd
        case aMsg of
            -- handle an event that someone need a neighbour.
            INeedNeighbors (toNodeId -> aNodeId) aHostAddress aPortNumber -> do
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info $ "Accepted msg from the " ++
                    show aNodeId ++ "that it need a neighbors. Addtition the node in the list of possible connects."

                C.writeChan (aData^.fileServerChan) $
                        FileActorRequestNetLvl $ UpdateFile (aData^.myNodeId) ( NodeInfoListNetLvl [(aNodeId, Connect aHostAddress aPortNumber)])

instance BroadcastProcessing (IORef ManagerNodeData) (BroadcastThingLvl LogicLvl) where
    processingOfBroadcast aMd aMsg = do
        aData <- readIORef aMd
        case aMsg of
            -- FIXME: rewrite show for shard, show hash only.
            -- handle broadcast received shard
            BroadcastShard aShard -> do
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info $ "Accepted new shard from broadcast. The shard is " ++ show aShard
                whenJust (aData^.shardingChan) $ \aChan ->
                    C.writeChan aChan $ T.NewShardInNetAction aShard

            -- handle received data that some node change its position.
            BroadcastPosition     aMyNodeId aNodePosition  -> do
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info $ "Accepted new position for the node." ++
                    "The node have position " ++ show aNodePosition ++ ", node id is " ++ show aMyNodeId
                C.writeChan (aData^.fileServerChan) $
                    FileActorRequestLogicLvl $ UpdateFile (aData^.myNodeId) (NodeInfoListLogicLvl [(toNodeId aMyNodeId, aNodePosition)])

                whenJust (aData^.nodes.at (toNodeId aMyNodeId)) $ \aNode ->
                    modifyIORef aMd $ nodes %~ M.insert (toNodeId aMyNodeId)
                        (aNode & nodePosition ?~ aNodePosition)

                sendToShardingLvl aData $
                    T.TheNodeHaveNewCoordinates (toNodeId aMyNodeId) aNodePosition

-- handle sharding layer broadcast message (where to put)
instance BroadcastProcessing (IORef ManagerNodeData) (BroadcastThingLvl MiningLvl) where
    processingOfBroadcast aMd aMsg = do
        aData <- readIORef aMd
        case aMsg of
            -- send network received PP messages
            BroadcastPPMsg aSenderType aBroadcastMsg aNodeType aIdFrom@(IdFrom aPPId) -> do
                aTime <- getTime Realtime

                when (aSenderType == PoW) $
                    modifyIORef aMd $ poWNodes %~ BI.insert aTime aPPId

                let aFilteredNode :: [InChan NNToPPMessage]
                    aFilteredNode = do
                        aNode <- snd <$> M.toList (aData^.ppNodes)
                        guard $ aNodeType == All || aNode^.ppType == aNodeType
                        return $ aNode^.ppChan

                C.writeChan (aData^.valueChan) (A.toJSON (MsgBroadcastMsg aBroadcastMsg (IdFrom aPPId)))

                forM_ aFilteredNode $ \aChan ->
                    writeChan aChan $ MsgBroadcastMsg aBroadcastMsg aIdFrom


            BroadcastPPMsgId aBroadcastMsg aIdFrom@(IdFrom aIdPPFrom) aIdTo@(IdTo aIdPPTo) -> do
                aTime <- getTime Realtime

                whenJust (aData^.ppNodes.at (aIdPPTo)) $ \aNode -> do
                    writeChan (aNode^.ppChan) $ MsgMsgToPP aIdPPTo aBroadcastMsg

            -- add new transaction in pending
            BroadcastTransaction aTransaction _ -> do
                C.writeChan (aData^.transactions) aTransaction

                -- logging and metrics
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
                    "Addtition the transaction to pending. The transaction = "
                        ++ show aTransaction
                writeMetric (aData^.infoMsgChan) $ add
                    ("net.node." ++ idShow (aData^.myNodeId) ++ ".pending.amount")
                    (1 :: Integer)

            -- add microblock to the shard
            BroadcastMicroBlock aMicroblock _ -> do
                sendToShardingLvl aData $
                    T.ShardAcceptAction (microblockToShard aMicroblock)
                C.writeChan (aData^.microblockChan) aMicroblock
                -- FIXME: rewrite show for Transaction and Microblock for showing only hash and structure
                C.writeChan (aData^.microblockChan) aMicroblock
                writeLog (aData^.infoMsgChan) [NetLvlTag] Info $
                    "Addtition the mickroblock to shard DB. The mickroblock = "
                    ++ show aMicroblock
            _ -> return ()

idShow :: Integral a => a -> String
idShow aMyNodeId = show (toInteger aMyNodeId)


-- | Make microblock from a shard
-- | It could be good to check that it has correct hash
--  TODO: check that block's hash corresponds to the block. Think, when it would be better to check.
microblockToShard :: Microblock -> Shard
microblockToShard _ = undefined
--    Shard ShardType (Hash "aaaa") (encode aMicroblock)

--------------------------------------------------------------------------------
