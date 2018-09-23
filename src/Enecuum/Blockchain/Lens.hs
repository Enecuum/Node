{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

-- | Lenses for Blockchain domain types.
module Enecuum.Blockchain.Lens where

import           Control.Lens             ( makeFieldsNoPrefix )

import           Enecuum.Blockchain.Domain

makeFieldsNoPrefix ''Transaction
