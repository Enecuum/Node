{-# LANGUAGE
        LambdaCase
    ,   ViewPatterns
    ,   MultiWayIf
    ,   ScopedTypeVariables
    ,   MultiParamTypeClasses
    ,   FlexibleContexts
    ,   PatternSynonyms
    ,   FlexibleInstances
    ,   TemplateHaskell
    ,   GADTs
  #-}

module Node.FileDB.FileServer (
        startFileServer
    ,   FileActorRequest(..)
    ,   FileActorRequestInternal(..)
  ) where

import              Lens.Micro.GHC()
import              Lens.Micro.Mtl()
import              Lens.Micro
import              System.Directory()
import              Lens.Micro.TH
import              Control.Concurrent.Chan
import              Control.Monad()
import qualified    Data.Map as M
import              Sharding.Space.Distance
import              Data.List.Extra

import              Node.Data.NetPackage
import              Node.FileDB.FileDB()
import              Node.Data.Key
import              Service.Network.Base
import              Sharding.Space.Point

data FileActor = FileActor {
        _filePositions :: M.Map NodeId NodePosition
    ,   _fileConnects  :: M.Map NodeId Connect
  }



makeLenses ''FileActor

data FileActorRequest where
    FileActorRequestNetLvl    :: FileActorRequestInternal NetLvl   -> FileActorRequest
    FileActorRequestLogicLvl  :: FileActorRequestInternal LogicLvl -> FileActorRequest
    FileActorMyPosition       :: MyNodePosition -> FileActorRequest


data FileActorRequestInternal a where
    ReadRecordsFromNodeListFile :: Chan (NodeInfoList a) -> FileActorRequestInternal a
    DeleteFromFile              :: NodeId -> FileActorRequestInternal a
    UpdateFile                  :: MyNodeId -> NodeInfoList a -> FileActorRequestInternal a



startFileServer :: Chan FileActorRequest -> IO ()
startFileServer chan = aLoop $ FileActor M.empty M.empty
  where
    aLoop aData =
        readChan chan >>= \case
            FileActorMyPosition aNodePosition -> do
                let aNewFilePositions = M.fromList . take 6 . sortOn (distanceTo aNodePosition.snd) . M.toList $ M.intersection
                        (aData^.filePositions) (aData^.fileConnects)
                    aNewFileConnects  = M.intersection (aData^.fileConnects) aNewFilePositions
                aLoop $ FileActor aNewFilePositions aNewFileConnects

            FileActorRequestNetLvl a -> case a of
                ReadRecordsFromNodeListFile aChan                   -> do
                    writeChan aChan $ NodeInfoListNetLvl $ M.toList (aData^.fileConnects)
                    aLoop aData
                DeleteFromFile aNodeId                              ->
                    aLoop $ aData & fileConnects %~ M.delete aNodeId
                UpdateFile aMyNodeId (NodeInfoListNetLvl aNodeInfoList)       ->
                    aLoop $ aData & fileConnects %~ M.union
                        (M.delete (toNodeId aMyNodeId) (M.fromList aNodeInfoList))

            FileActorRequestLogicLvl a -> case a of
                ReadRecordsFromNodeListFile aChan                   -> do
                    writeChan aChan $ NodeInfoListLogicLvl $ M.toList (aData^.filePositions)
                    aLoop aData
                DeleteFromFile aNodeId                              ->
                    aLoop $ aData & filePositions %~ M.delete aNodeId
                UpdateFile aMyNodeId (NodeInfoListLogicLvl aNodeInfoList)     ->
                    aLoop $ aData & filePositions %~ M.union
                        (M.delete (toNodeId aMyNodeId) (M.fromList aNodeInfoList))



--------------------------------------------------------------------------------
