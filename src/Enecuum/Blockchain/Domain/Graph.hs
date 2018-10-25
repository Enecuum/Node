{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- TODO: this is copy-paste from tests with little changes.

module Enecuum.Blockchain.Domain.Graph where

import Enecuum.Prelude

import qualified Data.HGraph.THGraph     as G

import qualified Enecuum.Core.HGraph.Internal.Types as T
import qualified Enecuum.Core.Language as L
import qualified Enecuum.Core.Types as D
import qualified Enecuum.Framework.Domain as D
import qualified Enecuum.Blockchain.Domain.Microblock as D
import qualified Enecuum.Blockchain.Domain.Transaction as D
import qualified Enecuum.Blockchain.Domain.KBlock as D
import           Enecuum.Core.HGraph.Interpreters.IO (runHGraphIO)
import           Enecuum.Core.HGraph.Internal.Impl (initHGraph)
import qualified Data.Serialize          as S
import qualified Data.ByteString.Base64  as Base64
import qualified Crypto.Hash.SHA256      as SHA
import           Data.HGraph.StringHashable (StringHash (..), StringHashable, toHash)

data NodeContent
  = KBlockContent D.KBlock
  | MBlockContent D.Microblock
  deriving (Show, Eq, Generic, ToJSON, FromJSON, Serialize)

instance StringHashable NodeContent where
  toHash (KBlockContent block)= toHash block
  toHash (MBlockContent block)= toHash block

type GraphVar  = D.TGraph NodeContent
type GraphL a  = L.HGraphL NodeContent a
type GraphNode = T.TNodeL NodeContent

initGraph :: IO GraphVar
initGraph = do
    graph <- initHGraph
    runHGraphIO graph $ L.newNode $ KBlockContent D.genesisKBlock
    pure graph

