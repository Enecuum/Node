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
  ) where

import              Control.Concurrent.MVar
import              Control.Concurrent.Chan.Unagi.Bounded
import qualified    Data.Set as S
import              Service.Network.Base


data FileActorRequest where
    ReadRecordsFromFile :: MVar [Connect]   -> FileActorRequest
    DeleteFromFile      :: Connect          -> FileActorRequest
    AddToFile           :: [Connect]        -> FileActorRequest

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

--------------------------------------------------------------------------------
