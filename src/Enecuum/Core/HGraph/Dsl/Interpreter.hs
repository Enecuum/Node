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

module Enecuum.Core.HGraph.Dsl.Interpreter where

import           Universum
import           Data.Serialize
import           Eff
import           Eff.Exc
import           Eff.Internal
import           Eff.SafeIO 

import           Enecuum.Core.HGraph.Dsl.Language
import           Enecuum.Core.HGraph.THGraph as G
import           Enecuum.Core.HGraph.StringHashable


instance ToNodeRef (DslTNode content) (TVar (THNode content))  where
    toNodeRef   = TNodeRef


instance ToNodeRef (DslTNode content) (HNodeRef (DslTNode content)) where
    toNodeRef = identity


instance ToNodeRef (DslTNode content) StringHash  where
    toNodeRef   = TNodeHash


instance StringHashable content => ToNodeRef (DslTNode content) content  where
    toNodeRef   = TNodeHash . toHash


instance Serialize c => Serialize (HNodeContent (DslTNode c))


instance (Serialize c, StringHashable c) => StringHashable (HNodeContent (DslTNode c)) where
    toHash (TNodeContent c) = toHash c


instance StringHashable c => ToContent (DslTNode c) c where
    toContent = TNodeContent
    fromContent (TNodeContent a) = a


type DslTNode content = DslHNode (TVar (THNode content)) content


data instance HNodeContent (DslHNode (TVar (THNode content)) content)
    = TNodeContent content
  deriving (Generic)


data instance HNodeRef     (DslHNode (TVar (THNode content)) content)
    = TNodeRef (TVar (THNode content))
    | TNodeHash StringHash
  deriving (Generic)


initHGraph :: StringHashable c => IO (TVar (THGraph c))
initHGraph = atomically G.newTHGraph


interpretHGraphDsl
    :: StringHashable c
    => TVar (THGraph c)
    -> HGraphDsl (DslTNode c) a
    -> Eff '[SIO, Exc SomeException] a

interpretHGraphDsl graph (NewNode x) =
    safeIO $ atomically $ W <$> G.newNode graph (fromContent x)

interpretHGraphDsl graph (GetNode x) = safeIO . atomically $ do
    aMaybeNode <- case x of
        TNodeRef aVar   -> return $ Just aVar
        TNodeHash aHash -> G.findNode graph aHash
    case aMaybeNode of
        Just aTNode -> do
            node <- readTVar aTNode
            return $ Just $ DslHNode 
                (toHash $ node^.content)
                (TNodeRef aTNode)
                (TNodeContent $ node^.content)
                (TNodeRef <$> node^.links)
                (TNodeRef <$> node^.rLinks) 
        Nothing -> return Nothing

interpretHGraphDsl graph (DeleteNode x) = safeIO . atomically $ case x of
    TNodeRef ref -> do
        G.deleteTHNode graph ref
        return $ W True
    TNodeHash hash -> W <$> G.deleteHNode graph hash

interpretHGraphDsl graph (NewLink x y) = safeIO . atomically $ case (x, y) of
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

interpretHGraphDsl graph (DeleteLink x y) = safeIO . atomically $ case (x, y) of
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


runHGraph :: StringHashable c => TVar (THGraph c) -> Eff '[HGraphDsl (DslTNode c)] w -> IO w
runHGraph _ (Val x) = return x
runHGraph aGraph (E u q) = case extract u of

    NewNode x   -> do
        aBool <- atomically $ G.newNode aGraph (fromContent x)
        runHGraph aGraph (qApp q (W aBool))

    DeleteNode  x -> do
        aBool <- atomically $ case x of
            TNodeRef aRef -> do
                G.deleteTHNode aGraph aRef
                return True
            TNodeHash aHash -> G.deleteHNode aGraph aHash

        runHGraph aGraph (qApp q (W aBool))

    NewLink    x y -> do
        aBool <- atomically $ case (x, y) of
            (TNodeRef  r1, TNodeRef  r2) -> G.newTLink r1 r2
            (TNodeHash r1, TNodeHash r2) -> G.newHLink aGraph r1 r2
            (TNodeRef  r1, TNodeHash r2) -> do
                aMaybeNode <- G.findNode aGraph r2
                case aMaybeNode of
                    Just aTNode -> G.newTLink r1 aTNode
                    Nothing     -> return False
            (TNodeHash  r1, TNodeRef r2) -> do
                aMaybeNode <- G.findNode aGraph r1
                case aMaybeNode of
                    Just aTNode -> G.newTLink aTNode r2
                    Nothing     -> return False
        runHGraph aGraph (qApp q (W aBool))

    DeleteLink x y -> do
        aBool <- atomically $ case (x, y) of
            (TNodeRef  r1, TNodeRef  r2) -> G.deleteTLink r1 r2
            (TNodeHash r1, TNodeHash r2) -> G.deleteHLink aGraph r1 r2
            (TNodeRef  r1, TNodeHash r2) -> do
                aMaybeNode <- G.findNode aGraph r2
                case aMaybeNode of
                    Just aTNode -> G.deleteTLink r1 aTNode
                    Nothing     -> return False
            (TNodeHash  r1, TNodeRef r2) -> do
                aMaybeNode <- G.findNode aGraph r1
                case aMaybeNode of
                    Just aTNode -> G.deleteTLink aTNode r2
                    Nothing     -> return False

        runHGraph aGraph (qApp q (W aBool))
    

    GetNode    x   -> do
        aRes <- atomically $ do
            aMaybeNode <- case x of
                TNodeRef aVar   -> return $ Just aVar
                TNodeHash aHash -> G.findNode aGraph aHash
            case aMaybeNode of
                Just aTNode -> do
                    aNode <- readTVar aTNode
                    return $ Just $ DslHNode 
                        (toHash $ aNode^.content)
                        (TNodeRef aTNode)
                        (TNodeContent $ aNode^.content)
                        (TNodeRef <$> aNode^.links)
                        (TNodeRef <$> aNode^.rLinks) 
                Nothing -> return Nothing
                
        runHGraph aGraph (qApp q aRes)



{-

        interpretNodeDefinitionL
  :: NodeRuntime
  -> L.NodeDefinitionL a
  -> Eff '[L.LoggerL, SIO, Exc SomeException] a
interpretNodeDefinitionL rt (L.NodeTag tag) = do
  L.logInfo $ "Node tag: " +| tag |+ ""
  safeIO $ atomically $ writeTVar (rt ^. RLens.tag) tag
interpretNodeDefinitionL rt (L.Initialization initScript) = do
  L.logInfo "Initialization"
  runNodeModel rt initScript
interpretNodeDefinitionL rt (L.Serving handlersF) = do
  L.logInfo "Serving handlersF"
  safeIO $ startNodeRpcServer rt handlersF

runNodeDefinitionL
  :: NodeRuntime
  -> Eff '[L.NodeDefinitionL, L.LoggerL, SIO, Exc SomeException] a
  -> Eff '[L.LoggerL, SIO, Exc SomeException] a
runNodeDefinitionL rt = handleRelay pure ( (>>=) . interpretNodeDefinitionL rt )
        -}