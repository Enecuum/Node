{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Enecuum.Framework.Node.Language where


import           Eff.TH                                   ( makeFreer )


data NodeL a where
  Dummy :: NodeL ()


makeFreer ''NodeL