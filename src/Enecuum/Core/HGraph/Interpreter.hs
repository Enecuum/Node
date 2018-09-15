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

module Enecuum.Core.HGraph.Interpreter (
      initHGraph
    , interpretHGraphL
    , runHGraphL
    , runHGraph
    ) where

import           Universum
import           Data.Serialize
import           Eff
import           Eff.Exc
import           Eff.SafeIO

import           Data.HGraph.THGraph as G
import           Data.HGraph.StringHashable (StringHash, StringHashable, toHash)
import           Enecuum.Core.HGraph.Language (HGraphModel, HGraphL (..))
import           Enecuum.Core.HGraph.Internal.Types (TNodeL)
import           Enecuum.Core.HGraph.Types (HNodeRef, HNode (..), HNodeContent, W (..),
                                            ToNodeRef, ToContent,
                                            fromContent, toContent, toNodeRef)

-- | Init HGraph.
initHGraph :: StringHashable c => IO (TVar (G.THGraph c))
initHGraph = atomically G.newTHGraph

-- | The interpreter of the language describing the action on graphs.
interpretHGraphL
    :: StringHashable c
    => TVar (G.THGraph c)
    -> HGraphL (TNodeL c) a
    -> Eff '[SIO, Exc SomeException] a

-- create a new node
interpretHGraphL graph (NewNode x) =
    safeIO $ atomically $ W <$> G.newNode graph (fromContent x)

-- get nodeby hash, content or ref
interpretHGraphL graph (GetNode x) = safeIO . atomically $ do
    mbNode <- case x of
        TNodeRef tNode     -> return $ Just tNode
        TNodeHash nodeHash -> G.findNode graph nodeHash
    case mbNode of
        Nothing    -> return Nothing
        Just tNode -> do
            node <- readTVar tNode
            return $ Just $ HNode
                (toHash $ node ^. content)
                (TNodeRef tNode)
                (TNodeContent $ node ^. content)
                (TNodeRef <$> node ^. links)
                (TNodeRef <$> node ^. rLinks)

-- delete node by hash, content or ref
interpretHGraphL graph (DeleteNode x) = safeIO . atomically $ case x of
    TNodeHash hash -> W <$> G.deleteHNode graph hash
    TNodeRef ref   -> G.deleteTHNode graph ref >> return (W True)

-- create new link by contents, hashes or refs of the node
interpretHGraphL graph (NewLink x y) = safeIO . atomically $ case (x, y) of
    (TNodeRef  r1, TNodeRef  r2) -> W <$> G.newTLink r1 r2
    (TNodeHash r1, TNodeHash r2) -> W <$> G.newHLink graph r1 r2
    (TNodeRef  r1, TNodeHash r2) -> G.findNode graph r2 >>= \case
        Just tNode -> W <$> G.newTLink r1 tNode
        Nothing    -> return $ W False
    (TNodeHash  r1, TNodeRef r2) -> G.findNode graph r1 >>= \case
        Just tNode -> W <$> G.newTLink tNode r2
        Nothing    -> return $ W False

-- delete link inter a nodes by contents, hashes or refs of the node
interpretHGraphL graph (DeleteLink x y) = safeIO . atomically $ case (x, y) of
    (TNodeRef  r1, TNodeRef  r2) -> W <$> G.deleteTLink r1 r2
    (TNodeHash r1, TNodeHash r2) -> W <$> G.deleteHLink graph r1 r2
    (TNodeRef  r1, TNodeHash r2) -> G.findNode graph r2 >>= \case
        Just tNode -> W <$> G.deleteTLink r1 tNode
        Nothing    -> return $ W False
    (TNodeHash  r1, TNodeRef r2) -> G.findNode graph r1 >>= \case
        Just tNode -> W <$> G.deleteTLink tNode r2
        Nothing    -> return $ W False

-- | Run H graph interpret.
runHGraphL
    :: StringHashable c
    => TVar (G.THGraph c)
    -> Eff (HGraphModel (TNodeL c)) w
    -> Eff '[SIO, Exc SomeException] w
runHGraphL graph = handleRelay pure ( (>>=) . interpretHGraphL graph )

-- | Run H graph interpret in IO monad.
runHGraph
    :: StringHashable c
    => TVar (G.THGraph c)
    -> Eff (HGraphModel (TNodeL c)) w
    -> IO w
runHGraph graph script = runSafeIO $ runHGraphL graph script

--------------------------------------------------------------------------------
-- INTERNAL
--------------------------------------------------------------------------------

instance ToNodeRef (TNodeL content) (TVar (THNode content))  where
    toNodeRef = TNodeRef

instance ToNodeRef (TNodeL content) (HNodeRef (TNodeL content)) where
    toNodeRef = identity

instance ToNodeRef (TNodeL content) StringHash  where
    toNodeRef = TNodeHash

instance StringHashable content => ToNodeRef (TNodeL content) content  where
    toNodeRef = TNodeHash . toHash

instance Serialize c => Serialize (HNodeContent (TNodeL c))

instance (Serialize c, StringHashable c) => StringHashable (HNodeContent (TNodeL c)) where
    toHash (TNodeContent c) = toHash c

instance StringHashable c => ToContent (TNodeL c) c where
    toContent = TNodeContent
    fromContent (TNodeContent a) = a

data instance HNodeContent (HNode (TVar (THNode content)) content)
    = TNodeContent content
  deriving (Generic)

data instance HNodeRef (HNode (TVar (THNode content)) content)
    = TNodeRef (TVar (THNode content))
    | TNodeHash StringHash
  deriving (Generic)
