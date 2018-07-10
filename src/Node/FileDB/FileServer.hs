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
    ,   getRecords
    ,   FileActorRequest(..)
  ) where

import              Control.Concurrent.MVar
import              Control.Concurrent.Chan.Unagi.Bounded
import qualified    Data.Set as S
import              Service.Network.Base
import              Service.Chan


data FileActorRequest where
    ReadRecordsFromFile :: MVar [Connect]   -> FileActorRequest
    DeleteFromFile      :: Connect          -> FileActorRequest
    AddToFile           :: [Connect]        -> FileActorRequest
    NumberConnects      :: MVar Int         -> FileActorRequest


getRecords :: InChan FileActorRequest -> IO [Connect]
getRecords aChan = do
    aTmpRef <- newEmptyMVar
    writeInChan aChan $ ReadRecordsFromFile aTmpRef
    takeMVar aTmpRef


startFileServer :: OutChan FileActorRequest -> IO ()
startFileServer aChan = aLoop S.empty
  where
    aLoop aConnects = readChan aChan >>= \case
        ReadRecordsFromFile aMVar -> do
            putMVar aMVar $ S.toList aConnects
            aLoop aConnects
        DeleteFromFile aConnect -> aLoop $
            S.delete aConnect aConnects
        AddToFile aNewConnects -> aLoop $
            S.union aConnects (S.fromList aNewConnects)
        NumberConnects aMVar -> do
            putMVar aMVar $ S.size aConnects
            aLoop aConnects

--------------------------------------------------------------------------------
