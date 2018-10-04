{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Enecuum.Assets.Nodes.NetworkNode where

import           Data.HGraph.StringHashable   (toHash)
import           Enecuum.Assets.Nodes.Address (nnAddr)
import qualified Enecuum.Domain               as D
import qualified Enecuum.Language             as L
import           Enecuum.Prelude

data TransactionRequest  = TransactionRequest { }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data TransactionResponse = TransactionResponse { tx :: D.Transaction }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

getTx:: TransactionRequest -> L.NodeL TransactionResponse
getTx TransactionRequest =  do
  L.logInfo "=="  
  undefined

nnNode :: L.NodeDefinitionL ()
nnNode = do
    L.nodeTag "Network Node node"
    L.logInfo "Generate Transactions"
    let (D.Address _ port) = nnAddr
    L.servingRpc port $ do
      L.method $ getTx

genTransaction :: Integer -> D.Transaction
genTransaction i =  D.Transaction
    { _owner     = i
    , _receiver  = i + 100
    , _amount    = i + 7
    }

