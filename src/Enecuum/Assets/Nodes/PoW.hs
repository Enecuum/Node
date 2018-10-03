{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

module Enecuum.Assets.Nodes.PoW where

import qualified Enecuum.Language              as L
import qualified Enecuum.Domain                as D
import           Enecuum.Prelude
import Enecuum.Assets.Nodes.Address (powAddr)


newtype KeyBlockRequest  = KeyBlockRequest { kBlock :: Int }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data KeyBlockResponse = KeyBlockResponse { kBlock :: [D.KBlock] }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

acceptKBlock :: KeyBlockRequest -> L.NodeL KeyBlockResponse
acceptKBlock (KeyBlockRequest q) = do
    -- L.getRandomInt (5,10)
    L.logInfo "Generate Key Block"
    pure undefined

-- powNode :: L.NodeDefinitionL ()
powNode = do
  L.nodeTag "PoW"
  L.logInfo "Generate Key Block"
  let (D.Address _ port) = powAddr
  L.servingRpc port $ do
    L.method $ acceptKBlock
  -- L.getRandomInt (5,10)
  -- replicateM 10 $ L.getRandomInt (5,10)
