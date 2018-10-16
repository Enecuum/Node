{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

-- | Lenses for Blockchain domain types.
module Enecuum.Blockchain.Lens where

import           Control.Lens             ( makeFieldsNoPrefix )
import Enecuum.Language (HasGraph)
import           Enecuum.Blockchain.Domain

makeFieldsNoPrefix ''Transaction
makeFieldsNoPrefix ''TransactionForSign
makeFieldsNoPrefix ''Microblock
makeFieldsNoPrefix ''KBlock
makeFieldsNoPrefix ''MicroblockForSign 
makeFieldsNoPrefix ''BlockchainData
