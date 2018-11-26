{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE TemplateHaskell        #-}

module Enecuum.Assets.Nodes.GraphNode.GraphServiceData where

import           Enecuum.Prelude

import           Enecuum.Assets.Nodes.GraphNode.Config
import qualified Enecuum.Blockchain.DB                 as D
import qualified Enecuum.Domain                        as D

data GraphServiceData = GraphServiceData
    { _blockchain          :: D.BlockchainData
    , _db                  :: Maybe D.DBModel
    , _dumpToDBSignal      :: D.StateVar Bool
    , _restoreFromDBSignal :: D.StateVar Bool
    , _checkPendingSignal  :: D.StateVar Bool
    }

makeFieldsNoPrefix ''GraphServiceData
