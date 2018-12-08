{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Enecuum.Samples.Blockchain.DB.Lens where

import           Enecuum.Prelude
import           Control.Lens (Getter, to, makeFieldsNoPrefix)

import           Enecuum.Samples.Blockchain.DB.Model 
import           Enecuum.Samples.Blockchain.DB.Entities
import qualified Enecuum.Core.Types        as D
import qualified Enecuum.Samples.Blockchain.Domain as D

makeFieldsNoPrefix ''DBModel


time' :: Getter (D.DBResult (D.DBValue KBlockEntity)) (D.DBResult D.BlockTime)
time' = to (\eVal -> eVal >>= (\(KBlockValue t _ _ _) -> Right t))

number' :: Getter (D.DBResult (D.DBValue KBlockEntity)) (D.DBResult D.BlockNumber)
number' = to (\eVal -> eVal >>= (\(KBlockValue _ n _ _) -> Right n))

nonce' :: Getter (D.DBResult (D.DBValue KBlockEntity)) (D.DBResult D.Nonce)
nonce' = to (\eVal -> eVal >>= (\(KBlockValue _ _ n _) -> Right n))

solver' :: Getter (D.DBResult (D.DBValue KBlockEntity)) (D.DBResult D.Solver)
solver' = to (\eVal -> eVal >>= (\(KBlockValue _ _ _ s) -> Right s))

prevHash' :: Getter (D.DBResult (D.DBValue KBlockPrevHashEntity)) (D.DBResult D.PrevHash)
prevHash' = to (\eVal -> eVal >>= (\(KBlockPrevHashValue ph) -> Right ph))
