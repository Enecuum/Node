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
module Node.Node.Mining (
    networkNodeStart
  ) where

import              System.Random()
import qualified    Crypto.PubKey.ECC.ECDSA         as ECDSA

import qualified    Data.Map                        as M
import qualified    Data.Bimap                      as BI
import              Data.Maybe (isNothing, catMaybes)
import              Data.List.Extra
import              System.Clock
import              Data.IORef
import              Data.Serialize
import              Lens.Micro
import              Lens.Micro.Mtl()
import qualified    Control.Concurrent              as C
import              Control.Concurrent.Chan.Unagi.Bounded
import              Control.Concurrent.MVar

import              Control.Monad.Extra
import              Crypto.Error

import              Node.FileDB.FileServer
import              Sharding.Types.ShardLogic
import              Service.Monad.Option
import              Node.Crypto
import              Node.Data.Key
import              Node.Node.Types
import qualified    Sharding.Types.Node as T
import              Service.InfoMsg
import              Node.Data.GlobalLoging
import              PoA.Types
import              Sharding.Sharding()
import              Node.BaseFunctions

networkNodeStart :: (InChan MsgToCentralActor, OutChan MsgToCentralActor) -> IORef NetworkNodeData -> IO ()
networkNodeStart (aChan, aOutChan) aMd = do
    aData <- readIORef aMd
    undead (writeLog (aData^.logChan) [NetLvlTag] Warning "networkNodeStart. This node could be die!" )
      $ forever $ do
          aData <- readIORef aMd
          readChan aOutChan >>= \case
            NodeIsDisconnected      aNodeId                   ->
                whenJust (aData^.connects.at aNodeId) $ \aNode -> do
                    writeLog (aData^.logChan) [NetLvlTag] Info $  "The node " ++ show aNodeId ++ " is disconnected."
                    modifyIORef aMd $ connects %~ M.delete aNodeId

            MsgFromNode  aMsgFromNode -> case aMsgFromNode of
                    AcceptedMicroblock aMicroblock aSenderId -> do
                        writeMetric (aData^.logChan) $ increment "net.bl.count"
                        writeLog (aData^.logChan) [NetLvlTag] Info $
                            "PP node " ++ show aSenderId ++ ", create a a microblock: " ++ show aMicroblock
                        writeChan (aData^.microblockChan) aMicroblock

                    NewConnect aNodeId aNodeType aChan  -> do
                        writeLog (aData^.logChan) [NetLvlTag] Info $
                            "A new connect with PP node " ++ show aNodeId ++ ", the type of node is " ++ show aNodeType
                        when (isNothing $ aData^.connects.at aNodeId) $
                            modifyIORef aMd $ connects %~ M.insert aNodeId
                                (NodeInfo aChan aNodeType)

                    BroadcastRequest aByteString aIdFrom aNodeType ->
                        forM_ (aData^.connects) $ \aNode -> when
                            (aNodeType == aNode^.nodeType || aNodeType == All) $
                            void $ tryWriteChan (aNode^.nodeChan) $ MsgBroadcastMsg aByteString aIdFrom

                    MsgResending (IdFrom aPPIdFrom) (IdTo aId) aByteString ->
                        whenJust (aData^.connects.at aId) $ \aNode ->
                            void $ tryWriteChan (aNode^.nodeChan) $
                                MsgMsgToPP aPPIdFrom aByteString


                    PoWListRequest (IdFrom aNodeId) ->
                        whenJust (aData^.connects.at aNodeId) $ \aNode -> do
                            let aPPIds = M.filter (\a -> a^.nodeType == PoW) (aData^.connects)
                            writeChan (aNode^.nodeChan) $ ResponsePoWList $ M.keys aPPIds
            MsgFromSharding         aShardingNodeRequestMsg   -> return ()
            CleanAction                                       -> return ()

            NewTransaction          aTransaction  -> do
                writeLog (aData^.logChan) [NetLvlTag] Info "I create a transaction."
                writeChan (aData^.transactionsChan) aTransaction


--------------------------------------------------------------------------------
