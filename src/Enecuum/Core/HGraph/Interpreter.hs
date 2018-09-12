{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ScopedTypeVariables    #-}

{-# OPTIONS_GHC -fno-warn-orphans   #-}

module Enecuum.Core.HGraph.Interpreter where

import           Universum
import           Data.Serialize
import           Eff
import           Eff.Exc
import           Eff.SafeIO 

import           Enecuum.Core.HGraph.Language
import           Enecuum.Core.HGraph.THGraph as G
import           Enecuum.Core.HGraph.StringHashable
import           Enecuum.Core.Logger.Language


instance ToNodeRef (TNodeL content) (TVar (THNode content))  where
    toNodeRef   = TNodeRef


instance ToNodeRef (TNodeL content) (HNodeRef (TNodeL content)) where
    toNodeRef = identity


instance ToNodeRef (TNodeL content) StringHash  where
    toNodeRef   = TNodeHash


instance StringHashable content => ToNodeRef (TNodeL content) content  where
    toNodeRef   = TNodeHash . toHash


instance Serialize c => Serialize (HNodeContent (TNodeL c))


instance (Serialize c, StringHashable c) => StringHashable (HNodeContent (TNodeL c)) where
    toHash (TNodeContent c) = toHash c


instance StringHashable c => ToContent (TNodeL c) c where
    toContent = TNodeContent
    fromContent (TNodeContent a) = a


type TNodeL content = HNodeL (TVar (THNode content)) content


data instance HNodeContent (HNodeL (TVar (THNode content)) content)
    = TNodeContent content
  deriving (Generic)


data instance HNodeRef     (HNodeL (TVar (THNode content)) content)
    = TNodeRef (TVar (THNode content))
    | TNodeHash StringHash
  deriving (Generic)


initHGraph :: StringHashable c => IO (TVar (THGraph c))
initHGraph = atomically G.newTHGraph

interpretHGraphL
    :: StringHashable c
    => TVar (THGraph c)
    -> HGraphL (TNodeL c) a
    -> Eff '[SIO, Exc SomeException] a

interpretHGraphL graph (NewNode x) =
    safeIO $ atomically $ W <$> G.newNode graph (fromContent x)

interpretHGraphL graph (GetNode x) = safeIO . atomically $ do
    aMaybeNode <- case x of
        TNodeRef aVar   -> return $ Just aVar
        TNodeHash aHash -> G.findNode graph aHash
    case aMaybeNode of
        Just aTNode -> do
            node <- readTVar aTNode
            return $ Just $ HNodeL 
                (toHash $ node^.content)
                (TNodeRef aTNode)
                (TNodeContent $ node^.content)
                (TNodeRef <$> node^.links)
                (TNodeRef <$> node^.rLinks) 
        Nothing -> return Nothing

interpretHGraphL graph (DeleteNode x) = safeIO . atomically $ case x of
    TNodeRef ref -> do
        G.deleteTHNode graph ref
        return $ W True
    TNodeHash hash -> W <$> G.deleteHNode graph hash

interpretHGraphL graph (NewLink x y) = safeIO . atomically $ case (x, y) of
    (TNodeRef  r1, TNodeRef  r2) -> W <$> G.newTLink r1 r2
    (TNodeHash r1, TNodeHash r2) -> W <$> G.newHLink graph r1 r2
    (TNodeRef  r1, TNodeHash r2) -> do
        aMaybeNode <- G.findNode graph r2
        case aMaybeNode of
            Just aTNode -> W <$> G.newTLink r1 aTNode
            Nothing     -> return $ W False
    (TNodeHash  r1, TNodeRef r2) -> do
        aMaybeNode <- G.findNode graph r1
        case aMaybeNode of
            Just aTNode -> W <$> G.newTLink aTNode r2
            Nothing     -> return $ W False

interpretHGraphL graph (DeleteLink x y) = safeIO . atomically $ case (x, y) of
    (TNodeRef  r1, TNodeRef  r2) -> W <$> G.deleteTLink r1 r2
    (TNodeHash r1, TNodeHash r2) -> W <$> G.deleteHLink graph r1 r2
    (TNodeRef  r1, TNodeHash r2) -> do
        aMaybeNode <- G.findNode graph r2
        case aMaybeNode of
            Just aTNode -> W <$> G.deleteTLink r1 aTNode
            Nothing     -> return $ W False
    (TNodeHash  r1, TNodeRef r2) -> do
        aMaybeNode <- G.findNode graph r1
        case aMaybeNode of
            Just aTNode -> W <$> G.deleteTLink aTNode r2
            Nothing     -> return $ W False

runHGraphL
    :: StringHashable c
    => TVar (THGraph c)
    -> Eff '[HGraphL (TNodeL c), SIO, Exc SomeException] w
    -> Eff '[SIO, Exc SomeException] w
runHGraphL rt = handleRelay pure ( (>>=) . interpretHGraphL rt )

runHGraph
    :: StringHashable c
    => TVar (THGraph c)
    -> Eff '[HGraphL (TNodeL c), SIO, Exc SomeException] w
    -> IO w
runHGraph graph script = runSafeIO $ runHGraphL graph script