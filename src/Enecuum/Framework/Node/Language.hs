{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Enecuum.Framework.Node.Language where

import           Eff.TH    ( makeFreer )

import qualified Enecuum.Framework.Domain as D

data NodeL a where
  Dummy :: NodeL a


makeFreer ''NodeL
