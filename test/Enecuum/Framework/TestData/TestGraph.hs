{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans   #-}

module Enecuum.Framework.TestData.TestGraph where

import Enecuum.Prelude

import qualified Data.ByteString.Base64  as Base64
import qualified Data.Serialize          as S
import qualified Data.Aeson              as A
import qualified Data.HGraph.THGraph     as G
import           Data.HGraph.StringHashable (StringHash, toHash)
import qualified Crypto.Hash.SHA256      as SHA

import           Enecuum.Prelude
import           Enecuum.Core.HGraph.Internal.Types

import qualified Enecuum.Language as L
import qualified Enecuum.Domain as D
import           Enecuum.Core.HGraph.Interpreter (initHGraph, runHGraph)

type LGraph = TVar (G.THGraph D.Transaction)

nilHash :: StringHash
nilHash = toHash (D.Transaction (toHash @Int 0) 0)

nilTransaction :: D.Transaction
nilTransaction = D.Transaction nilHash 0

initLGraph :: IO LGraph
initLGraph = do
    graph <- initHGraph
    runHGraph graph $ L.newNode nilTransaction
    return graph

withPrevState = error "withPrevState not implemented"

updateTransaction :: D.Transaction -> Int -> L.LGraphModel Bool
updateTransaction (D.Transaction prevHash change) = withPrevState prevHash $ \_ -> do
    error "updateTransaction not implemented"

getTransactionNode :: D.Transaction -> L.LGraphModel (Maybe (TNodeL D.Transaction))
getTransactionNode = L.getNode . toHash
