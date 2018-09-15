{-# OPTIONS_GHC -fno-warn-orphans   #-}

module Enecuum.Research.BGraph
    (   MBlock
    ,   KBlock
    ,   BGraph
    ,   BGraphDSL
    ,   TRef
    ,   NodeContent(..)
    ,   initBGraph
    ,   addKBlock
    ,   addMBlock
    ,   getMBlock
    ,   getKBlock
    ,   getLastKBlocks
    ) where

import qualified    Data.ByteString.Base64  as Base64
import qualified    Data.Serialize          as S

import qualified    Crypto.Hash.SHA256      as SHA

import              Enecuum.Prelude
import              Enecuum.Legacy.Refact.Hashing (calculateKeyBlockHash)
import              Enecuum.Legacy.Refact.Assets (genesisKeyBlock)
import              Enecuum.Legacy.Service.Types
    (   Microblock(..)
    ,   KeyBlockInfoPoW(..)
    )

import              Data.HGraph.THGraph (THGraph)
import              Data.HGraph.StringHashable
import              Enecuum.Core.HGraph.Language
import              Enecuum.Core.HGraph.Interpreter


type MBlock     = Microblock
type KBlock     = KeyBlockInfoPoW
type BGraph     = TVar      (THGraph    NodeContent)
type BGraphDSL  = HGraphL   (TNodeL   NodeContent)
type TRef       = HNodeRef  (TNodeL   NodeContent)

data NodeContent
    = MBlockContent MBlock
    | KBlockContent KBlock
  deriving (Generic)

instance S.Serialize NodeContent
instance StringHashable KBlock where
    toHash = StringHash . calculateKeyBlockHash

instance StringHashable MBlock where
    toHash = StringHash . Base64.encode . SHA.hash . S.encode

instance StringHashable NodeContent where
    toHash (MBlockContent x) = toHash x
    toHash (KBlockContent x) = toHash x



initBGraph :: IO BGraph
initBGraph = do
    aRes <- initHGraph
    runHGraph aRes $ newNode $ KBlockContent genesisKeyBlock
    return aRes

addKBlock :: KBlock -> Eff (HGraphModel (TNodeL NodeContent))  Bool
addKBlock = addBlock _prev_hash KBlockContent

addMBlock :: MBlock -> Eff (HGraphModel (TNodeL NodeContent)) Bool
addMBlock = addBlock _keyBlock MBlockContent

addBlock f1 f2 block = do
    aNode <- getNode (StringHash $ f1 block)
    case aNode of
        Just (HNode _ ref _ _ _) -> do
            newNode $ f2 block
            W aOk <- newLink' ref (f2 block)
            return aOk
        Nothing -> return False

getMBlock :: StringHash -> Eff (HGraphModel (TNodeL NodeContent)) (Maybe MBlock)
getMBlock hash = do
    aNode <- getNode hash
    case aNode of
        Just (HNode _ _ (fromContent -> MBlockContent block) _ _) ->
            return $ Just block
        _ -> return Nothing

getKBlock :: StringHash -> Eff (HGraphModel (TNodeL NodeContent)) (Maybe KBlock)
getKBlock hash = do
    aNode <- getNode hash
    case aNode of
        Just (HNode _ _ (fromContent -> KBlockContent block) _ _) ->
            return $ Just block
        _ -> return Nothing

-- O(n) - n is a number of kblock.
getLastKBlocks :: Eff (HGraphModel (TNodeL NodeContent)) [KBlock]
getLastKBlocks = do
    Just (HNode _ aRef _ _ _) <- getNode $ toHash genesisKeyBlock
    aRefs <- getLastKBlocks' aRef
    aKBlocks <- forM aRefs $ \aBRef -> do
        aNode <- getNode aBRef
        case aNode of
            Just (HNode _ _ (fromContent -> KBlockContent block) _ _) ->
                return $ Just block
            _ -> return Nothing
    return $ catMaybes aKBlocks

getLastKBlocks' :: TRef -> Eff (HGraphModel (TNodeL NodeContent)) [TRef]
getLastKBlocks' aRef = do
    aRefs <- getNextKBlocks' aRef
    if null aRefs
        then return [aRef]
        else concat <$> forM aRefs getLastKBlocks'

getNextKBlocks' :: TRef -> Eff (HGraphModel (TNodeL NodeContent)) [TRef]
getNextKBlocks' aRef = do
    Just (HNode _ _ _ _ rLinks) <- getNode aRef
    aRefs <- forM (elems rLinks) $ \aNRef -> do
        Just (HNode _ _ (fromContent -> block) _ _) <- getNode aNRef
        case block of
            KBlockContent _ -> return $ Just aNRef
            _               -> return Nothing
    return $ catMaybes aRefs
