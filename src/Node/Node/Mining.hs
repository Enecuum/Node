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
import              Control.Concurrent.MVar
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

            MsgFromNode aNodeType aMsgFromNode -> do
                void $ C.forkIO $ when (aNodeType /= NN) $ forM_ (aData^.connects) $
                    \aNode -> when (aNode^.nodeType == NN) $
                        void $ tryWriteChan (aNode^.nodeChan) aMsgFromNode

                case aMsgFromNode of
                    aMsg@(MsgBroadcast _ aNodeType _) ->
                        forM_ (aData^.connects) $ \aNode -> when
                            (aNode^.nodeType /= NN && (aNodeType == aNode^.nodeType || aNodeType == All)) $
                            void $ tryWriteChan (aNode^.nodeChan) aMsg

                    aMsg@(MsgMsgTo _ (IdTo aId) _) -> do
                        whenJust (aData^.connects.at aId) $ \aNode ->
                            void $ tryWriteChan (aNode^.nodeChan) aMsg

                    MsgMicroblock aMicroblock -> do
                        writeLog (aData^.logChan) [NetLvlTag] Info $
                             "Create a a microblock: " ++ show aMicroblock
                        void $ tryWriteChan (aData^.microblockChan) aMicroblock

                    MsgTransaction aTransaction -> do
                        aVar <- newEmptyMVar
                        void $ tryWriteChan (aData^.transactionsChan) (aTransaction, aVar)

                    MsgKeyBlock aKeyBlock -> do
                        void $ tryWriteChan (aData^.valueChan) aKeyBlock
                    a -> writeLog (aData^.logChan) [NetLvlTag] Info $  show a ++ " not a msg."

            ActionFromNode aMsgFromNode -> do
                case aMsgFromNode of
                    NewConnect aNodeId aNodeType aChan aConnect -> do
                        writeLog (aData^.logChan) [NetLvlTag] Info $
                            "A new connect with PP node " ++ show aNodeId ++ ", the type of node is " ++ show aNodeType
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
                writeLog (aData^.logChan) [NetLvlTag] Info "I create a transaction."
                void $ C.forkIO $ writeInChan (aData^.transactionsChan) (aTransaction, aVar)

--  data ActualConnectInfo = ActualConnectInfo NodeId NodeType (Maybe Connect) deriving Show

toActualConnectInfo :: (NodeId, NodeInfo) -> ActualConnectInfo
toActualConnectInfo (aNodeId, NodeInfo _ aNodeType aConnect) =
    ActualConnectInfo aNodeId aNodeType aConnect
--------------------------------------------------------------------------------
