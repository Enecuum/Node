{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans   #-}

module Enecuum.Framework.TestData.TestGraph where

import Enecuum.Prelude

import qualified Data.ByteString.Base64  as Base64
import qualified Data.Serialize          as S
import qualified Data.Aeson              as A
import qualified Crypto.Hash.SHA256      as SHA

import           Enecuum.Prelude

import qualified Enecuum.Core.HGraph.Language as L
import           Enecuum.Core.HGraph.Interpreter (initHGraph, runHGraph)
import           Enecuum.Core.HGraph.StringHashable

import qualified Enecuum.Framework.Domain.Types as T

nilHash :: StringHash
nilHash = toHash (0 :: Int)

nilTransaction :: T.Transaction
nilTransaction = T.Transaction nilHash 0

initLGraph :: IO T.LGraph
initLGraph = do
    graph <- initHGraph
    runHGraph graph $ L.newNode nilTransaction
    return graph

withPrevState = error "withPrevState not implemented"

updateTransaction :: T.Transaction -> Int -> Eff T.LGraphModel Bool
updateTransaction (T.Transaction prevHash change) = withPrevState prevHash $ \_ -> do
    error "updateTransaction not implemented"

getTransactionNode :: T.Transaction -> Eff T.LGraphModel (Maybe T.LGraphNode)
getTransactionNode = L.getNode . toHash
