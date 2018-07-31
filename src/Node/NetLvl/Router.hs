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
module Node.NetLvl.Router (routerActorStart) where


import              System.Random()

import              Service.Chan
import qualified    Data.Map                        as M
import              Data.Maybe (isNothing)
import              Data.Aeson as A
import              Data.IORef
import              Lens.Micro
import              Lens.Micro.Mtl()
import              Control.Concurrent.Chan.Unagi.Bounded
import              Control.Concurrent.MVar
import              Control.Monad.Extra
import              Node.Data.Key
import              Node.Node.Types
import              Service.InfoMsg
import              Node.Data.GlobalLoging
import              Node.NetLvl.Massages
import              Sharding.Sharding()
import              Node.BaseFunctions
import qualified    Control.Concurrent as C
import              Service.Sync.SyncJson


routerActorStart :: InChan SyncEvent -> (InChan MsgToCentralActor, OutChan MsgToCentralActor) -> IORef NetworkNodeData -> IO ()
routerActorStart aSyncChan (_, aOutChan) aMd = do
    aDataMain <- readIORef aMd
    let aWriteLog = writeLog (aDataMain^.logChan)
        aNetLog   = aWriteLog [NetLvlTag] Info
    aWriteLog [NetLvlTag, InitTag] Info "Init. Start of router actor level."
    undead (aWriteLog [NetLvlTag] Warning "networkNodeStart. This node could be die!" )
      $ forever $ do
          aData <- readIORef aMd
          readChan aOutChan >>= \case
            NodeIsDisconnected      aNodeId                   ->
                whenJust (aData^.connects.at aNodeId) $ \_ -> do
                    aNetLog $ "The node " ++ show aNodeId ++ " is disconnected."
                    modifyIORef aMd $ connects %~ M.delete aNodeId

            SendMsgToNode aMsgFromNode aIdTo@(IdTo aId) -> do
                whenJust (aData^.connects.at aId) $ \aNode -> do
                    aNetLog $ "Sending of msg to " ++ show aId
                    let MyNodeId aMyId = aData^.nodeConfig.myNodeId
                    void $ tryWriteChan (aNode^.nodeChan) $ MsgMsgTo (IdFrom $ NodeId aMyId) aIdTo aMsgFromNode

            MsgFromNode aNodeType aMsgFromNode -> do
                void $ C.forkIO $ when (aNodeType /= NN) $ forM_ (aData^.connects) $
                    \aNode -> when (aNode^.nodeType == NN) $
                        void $ C.forkIO $ writeInChan (aNode^.nodeChan) aMsgFromNode

                case aMsgFromNode of
                    aMsg@(MsgBroadcast _ aRecieverType _) -> do
                        aNetLog $ "Resending. Broadcast."
                        forM_ (aData^.connects) $ \aNode -> when
                            (aNode^.nodeType /= NN && (aRecieverType == aNode^.nodeType || aRecieverType == All)) $
                            void $ tryWriteChan (aNode^.nodeChan) aMsg

                    aMsg@(MsgMsgTo (IdFrom aSender) (IdTo aId) aContent) -> do
                      aNetLog $ "Received MsgMsgTo" ++ show aMsg
                      if aId == toNodeId (aData^.nodeConfig.myNodeId)
                      then do
                          aNetLog $ "Received Msg for me" ++ show aMsg
                          case fromJSON aContent of
                              Success aSyncMsg -> do
                                  aNetLog $ "Received sync msg. " ++ show aContent
                                  writeInChan aSyncChan $ SyncMsg aSender aSyncMsg
                              A.Error a -> do
                                  aNetLog $ "Error of parsing:" ++ a
                      else whenJust (aData^.connects.at aId) $ \aNode ->
                              void $ tryWriteChan (aNode^.nodeChan) aMsg

                    aMsg@(MsgMicroblock aMicroblock) -> do
                        aNetLog $ "Create a a microblock: " ++ show aMicroblock
                        void $ tryWriteChan (aData^.microblockChan) aMicroblock
                        void $ C.forkIO $ forM_ (aData^.connects) $ \aNode -> when
                            (aNode^.nodeType == All) $
                            writeInChan (aNode^.nodeChan) aMsg

                    MsgTransaction aTransaction -> do
                        aVar <- newEmptyMVar
                        void $ tryWriteChan (aData^.transactionsChan) (aTransaction, aVar)

                    aMsg@(MsgKeyBlock aKeyBlock) -> do
                        aNetLog "Received key block"
                        void $ tryWriteChan (aData^.valueChan) aKeyBlock
                        void $ C.forkIO $ forM_ (aData^.connects) $ \aNode -> when
                            (aNode^.nodeType /= NN) $ do
                                aNetLog "Key block is resended."
                                writeInChan (aNode^.nodeChan) aMsg

                    a -> aNetLog $  show a ++ " not a msg."


            ActionFromNode aMsgFromNode -> do
                case aMsgFromNode of
                    NewConnect aNodeId aNodeType aChan aConnect -> do
                        aNetLog $ "A new connect with PP node " ++ show aNodeId ++ ", the type of node is " ++ show aNodeType
                        when (isNothing $ aData^.connects.at aNodeId) $
                            modifyIORef aMd $ connects %~ M.insert aNodeId
                                (NodeInfo aChan aNodeType aConnect)
                    RequestListOfPoW (IdFrom aNodeId) ->
                        whenJust (aData^.connects.at aNodeId) $ \aNode -> do
                            let aPPIds = M.toList $ M.filter (\a -> a^.nodeType == PoW) (aData^.connects)
                            void $ tryWriteChan (aNode^.nodeChan) $ ResponsePoWList $ toActualConnectInfo <$> aPPIds

                    RequestActualConnectList aVar -> void $ C.forkIO $ do
                        let aConnects = toActualConnectInfo <$> (M.toList $ aData^.connects)
                        C.putMVar aVar aConnects

            ActualConnectsToNNRequest aVar -> void $ C.forkIO $ do
                let aConnects = toActualConnectInfo <$> (M.toList $ aData^.connects)
                C.putMVar aVar (filter (\(ActualConnectInfo _ aNodeType  _) -> aNodeType == NN) aConnects)

            MsgFromSharding         _   -> return ()
            CleanAction                 -> return ()

            NewTransaction          aTransaction aVar  -> do
                aNetLog "I create a transaction."
                void $ C.forkIO $ writeInChan (aData^.transactionsChan) (aTransaction, aVar)

--  data ActualConnectInfo = ActualConnectInfo NodeId NodeType (Maybe Connect) deriving Show

toActualConnectInfo :: (NodeId, NodeInfo) -> ActualConnectInfo
toActualConnectInfo (aNodeId, NodeInfo _ aNodeType aConnect) =
    ActualConnectInfo aNodeId aNodeType aConnect
--------------------------------------------------------------------------------
