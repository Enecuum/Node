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
import              Control.Monad.Free
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
import              Enecuum.Core.HGraph.Types
import              Enecuum.Core.HGraph.Interpreters.IO
import              Enecuum.Core.HGraph.Internal.Types
import              Enecuum.Core.HGraph.Internal.Impl (initHGraph)

type MBlock     = Microblock
type KBlock     = KeyBlockInfoPoW
type BGraph     = TGraph NodeContent
type BGraphDSL  = HGraphF   (TNodeL   NodeContent)
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

type BGraphL a = Free (HGraphF (TNodeL NodeContent)) a

initBGraph :: IO BGraph
initBGraph = do
    aRes <- initHGraph
    runHGraphIO aRes $ newNode $ KBlockContent genesisKeyBlock
    pure aRes

addKBlock :: KBlock -> BGraphL Bool
addKBlock = addBlock _prev_hash KBlockContent

addMBlock :: MBlock -> BGraphL Bool
addMBlock = addBlock _keyBlock MBlockContent

addBlock f1 f2 block = do
    aNode <- getNode (StringHash $ f1 block)
    case aNode of
        Just (HNode _ ref _ _ _) -> do
            newNode $ f2 block
            newLink' ref (f2 block)
        Nothing -> pure False

getMBlock :: StringHash -> BGraphL (Maybe MBlock)
getMBlock hash = do
    aNode <- getNode hash
    case aNode of
        Just (HNode _ _ (fromContent -> MBlockContent block) _ _) -> pure $ Just block
        _ -> pure Nothing

getKBlock :: StringHash -> BGraphL (Maybe KBlock)
getKBlock hash = do
    aNode <- getNode hash
    case aNode of
        Just (HNode _ _ (fromContent -> KBlockContent block) _ _) -> pure $ Just block
        _ -> pure Nothing

-- O(n) - n is a number of kblock.
getLastKBlocks :: BGraphL [KBlock]
getLastKBlocks = do
    Just (HNode _ aRef _ _ _) <- getNode $ toHash genesisKeyBlock
    aRefs                     <- getLastKBlocks' aRef
    aKBlocks                  <- forM aRefs $ \aBRef -> do
        aNode <- getNode aBRef
        case aNode of
            Just (HNode _ _ (fromContent -> KBlockContent block) _ _) -> pure $ Just block
            _ -> pure Nothing
    pure $ catMaybes aKBlocks

getLastKBlocks' :: TRef -> BGraphL [TRef]
getLastKBlocks' aRef = do
    aRefs <- getNextKBlocks' aRef
    if null aRefs then pure [aRef] else concat <$> forM aRefs getLastKBlocks'

getNextKBlocks' :: TRef -> BGraphL [TRef]
getNextKBlocks' aRef = do
    Just (HNode _ _ _ _ rLinks) <- getNode aRef
    aRefs                       <- forM (elems rLinks) $ \aNRef -> do
        Just (HNode _ _ (fromContent -> block) _ _) <- getNode aNRef
        case block of
            KBlockContent _ -> pure $ Just aNRef
            _               -> pure Nothing
    pure $ catMaybes aRefs
