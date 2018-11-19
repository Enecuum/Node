{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# OPTIONS  -Wno-orphans           #-}

-- | Lenses for Blockchain domain types.
module Enecuum.Blockchain.Lens where

import           Control.Lens                     (makeFieldsNoPrefix)
import           Enecuum.Framework.Language.Extra (HasGraph(..))
import           Enecuum.Blockchain.Domain

makeFieldsNoPrefix ''Transaction
makeFieldsNoPrefix ''Microblock
makeFieldsNoPrefix ''KBlock
makeFieldsNoPrefix ''BlockchainData
