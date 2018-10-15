{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Enecuum.Assets.Nodes.NetworkNode where

import           Enecuum.Assets.Nodes.Address (nnAddr)
import qualified Enecuum.Domain               as D
import qualified Enecuum.Language             as L
import           Enecuum.Prelude

data TransactionRequest  = TransactionRequest { }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data TransactionResponse = TransactionResponse { transaction :: [D.Transaction] }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


getTx :: TransactionRequest -> L.NodeL TransactionResponse
getTx TransactionRequest = do
    tx <- D.genNTransactions 10
    pure $ TransactionResponse tx

nnNode :: L.NodeDefinitionL ()
nnNode = do
    L.nodeTag "Network Node"
    L.logInfo "Generate Transactions"
    let (D.Address _ port) = nnAddr
    L.serving D.Rpc port $ L.method getTx
    L.awaitNodeForever
