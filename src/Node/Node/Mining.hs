{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Node.Node.Mining (
    networkNodeStart
  ) where


import              System.Random()

import              Service.Chan
import qualified    Data.Map                        as M
import              Data.Maybe (isNothing)
import              Data.IORef
import              Lens.Micro
import              Lens.Micro.Mtl()
import              Control.Concurrent.Chan.Unagi.Bounded
import              Control.Monad.Extra
import              Node.Data.Key
import              Node.Node.Types
import              Service.InfoMsg
import              Node.Data.GlobalLoging
import              PoA.Types
import              Sharding.Sharding()
import              Node.BaseFunctions
import qualified    Control.Concurrent as C


networkNodeStart :: (InChan MsgToCentralActor, OutChan MsgToCentralActor) -> IORef NetworkNodeData -> IO ()
networkNodeStart (_, aOutChan) aMd = do
    aDataMain <- readIORef aMd
    undead (writeLog (aDataMain^.logChan) [NetLvlTag] Warning "networkNodeStart. This node could be die!" )
      $ forever $ do
          aData <- readIORef aMd
          readChan aOutChan >>= \case
            NodeIsDisconnected      aNodeId                   ->
                whenJust (aData^.connects.at aNodeId) $ \_ -> do
                    writeLog (aData^.logChan) [NetLvlTag] Info $  "The node " ++ show aNodeId ++ " is disconnected."
                    modifyIORef aMd $ connects %~ M.delete aNodeId

            MsgFromNode  aMsgFromNode -> case aMsgFromNode of
                    AcceptedMicroblock aMicroblock -> do
                        writeMetric (aData^.logChan) $ increment "net.bl.count"
                        writeLog (aData^.logChan) [NetLvlTag] Info $
                             "create a a microblock: " ++ show aMicroblock
                        void $ tryWriteChan (aData^.microblockChan) aMicroblock

                    NewConnect aNodeId aNodeType aChan aConnect -> do
                        writeLog (aData^.logChan) [NetLvlTag] Info $
                            "A new connect with PP node " ++ show aNodeId ++ ", the type of node is " ++ show aNodeType
                        when (isNothing $ aData^.connects.at aNodeId) $
                            modifyIORef aMd $ connects %~ M.insert aNodeId
                                (NodeInfo aChan aNodeType aConnect)

                    ResendingBroadcast aBroadcastMsg aIdFrom aNodeType -> do
                        void $ tryWriteChan (aData^.valueChan) aBroadcastMsg
                        forM_ (aData^.connects) $ \aNode -> when
                            (aNodeType == aNode^.nodeType || aNodeType == All) $
                            void $ tryWriteChan (aNode^.nodeChan) $ MsgBroadcast aIdFrom aNodeType aBroadcastMsg 

                    ResendingMsgTo aIdFrom aTo@(IdTo aId) aByteString ->
                        whenJust (aData^.connects.at aId) $ \aNode ->
                            void $ tryWriteChan (aNode^.nodeChan) $
                                MsgMsgTo aIdFrom aTo aByteString


                    RequestListOfPoW (IdFrom aNodeId) ->
                        whenJust (aData^.connects.at aNodeId) $ \aNode -> do
                            let aPPIds = M.toList $ M.filter (\a -> a^.nodeType == PoW) (aData^.connects)
                            void $ tryWriteChan (aNode^.nodeChan) $ ResponsePoWList $ toActualConnectInfo <$> aPPIds

                    RequestActualConnectList aVar -> void $ C.forkIO $ do
                        let aConnects = toActualConnectInfo <$> (M.toList $ aData^.connects)
                        C.putMVar aVar aConnects


            MsgFromSharding         _   -> return ()
            CleanAction                 -> return ()

            NewTransaction          aTransaction aVar  -> do
                writeLog (aData^.logChan) [NetLvlTag] Info "I create a transaction."
                void $ C.forkIO $ writeInChan (aData^.transactionsChan) (aTransaction, aVar)


toActualConnectInfo :: (NodeId, NodeInfo) -> ActualConnectInfo
toActualConnectInfo (aNodeId, NodeInfo _ aNodeType aConnect) =
    ActualConnectInfo aNodeId aNodeType aConnect
--------------------------------------------------------------------------------
