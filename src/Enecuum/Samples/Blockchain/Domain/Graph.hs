{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Samples.Blockchain.Domain.Graph where

import           Data.HGraph.StringHashable           (StringHashable, toHash)

import qualified Enecuum.Samples.Blockchain.Domain.KBlock     as D
import qualified Enecuum.Samples.Blockchain.Domain.Microblock as D

import           Enecuum.Core.HGraph.Internal.Impl    (initHGraph)
import qualified Enecuum.Core.HGraph.Internal.Types   as T
import           Enecuum.Core.HGraph.Interpreters.IO  (runHGraphIO)
import qualified Enecuum.Core.Language                as L
import qualified Enecuum.Core.Types                   as D
import           Enecuum.Prelude

data NodeContent
    = KBlockContent D.KBlock
    | MBlockContent D.Microblock
    deriving (Show, Eq, Generic, ToJSON, FromJSON, Serialize)

instance StringHashable NodeContent where
    toHash (KBlockContent block) = toHash block
    toHash (MBlockContent block) = toHash block

type GraphVar  = D.TGraph NodeContent
type GraphL a  = L.HGraphL NodeContent a
type GraphNode = T.TNodeL NodeContent

initGraph :: IO GraphVar
initGraph = do
    graph <- initHGraph
    runHGraphIO graph $ L.newNode $ KBlockContent D.genesisKBlock
    pure graph

isKBlockNode :: GraphNode -> Bool
isKBlockNode (D.HNode _ _ (D.fromContent -> KBlockContent _) _ _) = True
isKBlockNode _                                                    = False

isMBlockNode :: GraphNode -> Bool
isMBlockNode (D.HNode _ _ (D.fromContent -> MBlockContent _) _ _) = True
isMBlockNode _                                                    = False
