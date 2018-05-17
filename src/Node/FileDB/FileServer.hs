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
import              Data.Map as M

import              Node.Data.NetPackage
import              Node.FileDB.FileDB()
import              Node.Data.Key
import              Service.Network.Base
import              Sharding.Space.Point

data FileActor = FileActor {
        _filePositions :: Map NodeId NodePosition
    ,   _fileConnects  :: Map NodeId Connect
  }



makeLenses ''FileActor

data FileActorRequest where
    FileActorRequestNetLvl    :: FileActorRequestInternal NetLvl   -> FileActorRequest
    FileActorRequestLogicLvl  :: FileActorRequestInternal LogicLvl -> FileActorRequest


data FileActorRequestInternal a where
    ReadRecordsFromNodeListFile :: Chan (NodeInfoList a) -> FileActorRequestInternal a
    DeleteFromFile              :: NodeId -> FileActorRequestInternal a
    UpdateFile                  :: MyNodeId -> NodeInfoList a -> FileActorRequestInternal a

{-
class FileDB a where
    readRecordsFromNodeListFile :: IO (NodeInfoList a)
    addRecordsToNodeListFile    :: MyNodeId -> NodeInfoList a -> IO ()
    deleteFromFile              :: a -> NodeId -> IO ()
    updateFile                  :: NodeInfoList a -> IO ()
-}

startFileServer :: Chan FileActorRequest -> IO ()
startFileServer chan = aLoop $ FileActor M.empty M.empty
  where
    aLoop aData =
        readChan chan >>= \case
            FileActorRequestNetLvl a -> case a of
                ReadRecordsFromNodeListFile aChan                   -> do
                    writeChan aChan $ NodeInfoListNetLvl $ M.toList (aData^.fileConnects)
                    aLoop aData
                DeleteFromFile aNodeId                              ->
                    aLoop $ aData & fileConnects %~ delete aNodeId
                UpdateFile aMyNodeId (NodeInfoListNetLvl aNodeInfoList)       ->
                    aLoop $ aData & fileConnects %~ union
                        (delete (toNodeId aMyNodeId) (M.fromList aNodeInfoList))

            FileActorRequestLogicLvl a -> case a of
                ReadRecordsFromNodeListFile aChan                   -> do
                    writeChan aChan $ NodeInfoListLogicLvl $ M.toList (aData^.filePositions)
                    aLoop aData
                DeleteFromFile aNodeId                              ->
                    aLoop $ aData & filePositions %~ delete aNodeId
                UpdateFile aMyNodeId (NodeInfoListLogicLvl aNodeInfoList)     ->
                    aLoop $ aData & filePositions %~ union
                        (delete (toNodeId aMyNodeId) (M.fromList aNodeInfoList))


--instance FileDB NetLvl where
{-
    readRecordsFromNodeListFile =
        NodeInfoListNetLvl <$> readDataFile "./data/listOfConnects.txt"


    addRecordsToNodeListFile aMyNodeId (NodeInfoListNetLvl aList) = do
        NodeInfoListNetLvl aFileContent <- readRecordsFromNodeListFile
        let aNotInFile  = (`notElem` aFileContent)
            aNotIAm  a  = toMyNodeId (a^._1) /= aMyNodeId

        addDataToFile "./data/listOfConnects.txt" $
            filter (\a -> aNotInFile a && aNotIAm a) aList


    deleteFromFile _ aNodeId = do
        NodeInfoListNetLvl aRecords <-readRecordsFromNodeListFile
        let aFilteredRecords = filter (\a -> a^._1 /= aNodeId) aRecords
        writeDataToFile "./data/listOfConnects.txt" aFilteredRecords

    updateFile (NodeInfoListNetLvl aNewRecords) = do
        NodeInfoListNetLvl aRecords <-readRecordsFromNodeListFile
        let aIdsForUpdate    = (^._1) <$> aNewRecords
            aFilteredRecords = filter (\a -> a^._1 `notElem` aIdsForUpdate) aRecords
            aUpdatedRecords  = aNewRecords ++ aFilteredRecords

        writeDataToFile "./data/listOfConnects.txt" aUpdatedRecords
-}

--instance FileDB LogicLvl where
{-
    readRecordsFromNodeListFile =
        NodeInfoListLogicLvl <$> readDataFile "./data/listOfPositions.txt"


    addRecordsToNodeListFile aMyNodeId (NodeInfoListLogicLvl aList) = do
        NodeInfoListLogicLvl aFileContent <- readRecordsFromNodeListFile
        let aFilteredRecords = filter
                (\a -> aNotInFile a && aNotIAm a) aList
            aNotInFile      a = a `notElem` aFileContent
            aNotIAm         a = toMyNodeId (a^._1) /= aMyNodeId

        addDataToFile "./data/listOfPositions.txt" aFilteredRecords


    deleteFromFile _ aNodeId = do
        NodeInfoListLogicLvl aRecords <- readRecordsFromNodeListFile
        let aFilteredRecords = filter (\a -> a^._1 /= aNodeId) aRecords
        writeDataToFile "./data/listOfPositions.txt" aFilteredRecords


    updateFile (NodeInfoListLogicLvl aNewRecords) = do
        NodeInfoListLogicLvl aRecords <-readRecordsFromNodeListFile
        let aIdsForUpdate    = (^._1) <$> aNewRecords
            aFilteredRecords = filter (\a -> a^._1 `notElem` aIdsForUpdate) aRecords
            aUpdatedRecords  = aNewRecords ++ aFilteredRecords

        writeDataToFile "./data/listOfPositions.txt" aUpdatedRecords


-}

--------------------------------------------------------------------------------
