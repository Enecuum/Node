{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE TemplateHaskell        #-}

module Enecuum.Samples.Assets.Nodes.GraphService.GraphServiceData where

import           Enecuum.Prelude

import           Enecuum.Samples.Assets.Nodes.GraphService.Config
import qualified Enecuum.Samples.Blockchain.DB                    as D
import qualified Enecuum.Domain                           as D
import           Enecuum.Samples.Blockchain.Domain          as D

data GraphServiceData = GraphServiceData
    { _blockchain          :: D.BlockchainData
    , _db                  :: Maybe D.DBModel
    , _dumpToDBSignal      :: D.StateVar Bool
    , _restoreFromDBSignal :: D.StateVar Bool
    , _checkPendingSignal  :: D.StateVar Bool
    }

makeFieldsNoPrefix ''GraphServiceData
