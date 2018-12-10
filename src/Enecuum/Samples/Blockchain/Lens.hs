{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS  -Wno-orphans           #-}

-- | Lenses for Blockchain domain types.
module Enecuum.Samples.Blockchain.Lens where

import           Control.Lens                     (makeFieldsNoPrefix)
import           Data.HGraph.StringHashable       (StringHash)
import           Enecuum.Samples.Blockchain.Domain
import           Enecuum.Samples.Blockchain.Domain.KBlock (BlockNumber)
import           Enecuum.Core.Types               (StateVar)
import           Enecuum.Framework.Language.Extra (HasGraph (..))
import           Enecuum.Prelude

makeFieldsNoPrefix ''Transaction
makeFieldsNoPrefix ''Microblock
makeFieldsNoPrefix ''KBlock
makeFieldsNoPrefix ''WindowedGraph
makeFieldsNoPrefix ''BlockchainData

--
-- -- Short lenses: BlockchainData -> WindowedGraph

wGraph :: Lens' BlockchainData GraphVar
wGraph = windowedGraph . graph

-- wWindowSize :: Lens' BlockchainData (StateVar BlockNumber)
-- wWindowSize = windowedGraph . windowSize

wBottomKBlockHash :: Lens' BlockchainData (StateVar StringHash)
wBottomKBlockHash = windowedGraph . bottomKBlockHash

wTopKBlockHash :: Lens' BlockchainData (StateVar StringHash)
wTopKBlockHash = windowedGraph . topKBlockHash
