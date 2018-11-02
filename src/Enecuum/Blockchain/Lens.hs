{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# OPTIONS  -Wno-orphans           #-}

-- | Lenses for Blockchain domain types.
module Enecuum.Blockchain.Lens where

import           Control.Lens                     ( makeFieldsNoPrefix )
import           Enecuum.Framework.Language.Extra (HasGraph(..))
import           Enecuum.Blockchain.Domain
import           Enecuum.Blockchain.DB.Model      (DBModel)

makeFieldsNoPrefix ''Transaction
makeFieldsNoPrefix ''TransactionForSign
makeFieldsNoPrefix ''Microblock
makeFieldsNoPrefix ''KBlock
makeFieldsNoPrefix ''MicroblockForSign 
makeFieldsNoPrefix ''BlockchainData
makeFieldsNoPrefix ''DBModel
