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

module Node.DataActor (
        startDataActor
    ,   getRecords
    ,   DataActorRequest(..)
  ) where

import              Control.Concurrent.MVar
import              Control.Concurrent.Chan.Unagi.Bounded
import qualified    Data.Set as S
import              Service.Network.Base
import              Service.Chan


data DataActorRequest a where
    ReadRecords     :: MVar [a]   -> DataActorRequest a
    DeleteRecords   :: a          -> DataActorRequest a
    AddRecords      :: [a]        -> DataActorRequest a
    NumberOfRecords :: MVar Int   -> DataActorRequest a


getRecords :: InChan (DataActorRequest a) -> IO [a]
getRecords aChan = do
    aTmpRef <- newEmptyMVar
    writeInChan aChan $ ReadRecords aTmpRef
    takeMVar aTmpRef


startDataActor :: Ord a => OutChan (DataActorRequest a) -> IO ()
startDataActor aChan = aLoop S.empty
  where
    aLoop aConnects = readChan aChan >>= \case
        ReadRecords aMVar -> do
            putMVar aMVar $ S.toList aConnects
            aLoop aConnects
        DeleteRecords aConnect -> aLoop $
            S.delete aConnect aConnects
        AddRecords aNewConnects -> aLoop $
            S.union aConnects (S.fromList aNewConnects)
        NumberOfRecords aMVar -> do
            putMVar aMVar $ S.size aConnects
            aLoop aConnects

--------------------------------------------------------------------------------
