{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Enecuum.Legacy.Node.DataActor (
        startDataActor
    ,   getRecords
    ,   DataActorRequest(..)
    ,   startContainerActor
    ,   ContainerCommands(..)
  ) where


import           Control.Concurrent.Chan.Unagi.Bounded
-- import           Control.Concurrent.MVar
import qualified Data.Set                              as S
import           Enecuum.Legacy.Service.Chan
import           Universum

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
    aLoop aData = readChan aChan >>= \case
        ReadRecords aMVar -> do
            putMVar aMVar $ S.toList aData
            aLoop aData
        DeleteRecords aConnect -> aLoop $
            S.delete aConnect aData
        AddRecords aNewConnects -> aLoop $
            S.union aData (S.fromList aNewConnects)
        NumberOfRecords aMVar -> do
            putMVar aMVar $ S.size aData
            aLoop aData

--------------------------------------------------------------------------------

data ContainerCommands m a where
    -- null, empty, member
    Predicate :: (m a -> Bool)    -> MVar Bool      -> ContainerCommands m a
    Lookup    :: (m a -> Maybe a) -> MVar (Maybe a) -> ContainerCommands m a
    -- insert, delete, adjust, updateWithKey
    Update    :: (m a -> m a)                       -> ContainerCommands m a
    -- size, length
    Size      :: (m a -> Int)     -> MVar Int       -> ContainerCommands m a


startContainerActor :: m a -> OutChan (ContainerCommands m a) -> IO ()
startContainerActor aEmpty aChan = aLoop aEmpty
  where
    aLoop aData = readChan aChan >>= \case
        -- null, empty, member
        Predicate f aVar -> putMVar aVar (f aData) >> aLoop aData
        Lookup    f aVar -> putMVar aVar (f aData) >> aLoop aData
        -- insert, delete, adjust, updateWithKey
        Update    f      -> aLoop (f aData)
        -- size, length
        Size      f aVar -> putMVar aVar (f aData) >> aLoop aData
