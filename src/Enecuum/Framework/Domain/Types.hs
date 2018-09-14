{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Enecuum.Framework.Domain.Types where

import Enecuum.Prelude

import qualified Data.ByteString.Base64  as Base64
import qualified Data.Serialize          as S
import qualified Crypto.Hash.SHA256      as SHA

import           Enecuum.Core.HGraph.StringHashable (StringHash (..), StringHashable, toHash)
import qualified Enecuum.Core.HGraph.Language as L
import           Enecuum.Core.HGraph.Interpreter (TNodeL)
import           Enecuum.Core.HGraph.THGraph (THGraph)

-- This data structure is for tests of graph incorporation only.
-- Please, replace it by actual blockchain data.

type TransactionID = Int

data Transaction = Transaction
    { _prevHash    :: StringHash
    , _change      :: Int
    }
  deriving (Generic)

instance S.Serialize Transaction

instance StringHashable Transaction where
    toHash = StringHash . Base64.encode . SHA.hash . S.encode

type LGraphNode = TNodeL Transaction
type LGraph     = TVar (THGraph Transaction)
type LGraphL    = L.HGraphL LGraphNode
type LGraphRef  = L.HNodeRef LGraphNode

type LGraphModel = L.HGraphModel LGraphNode