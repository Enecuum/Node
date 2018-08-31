{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Enecuum.Framework.Networking.Language where

import           Eff.TH                                   ( makeFreer )
import           Eff                                      ( Eff )

import qualified Enecuum.Framework.Domain      as D

import           Enecuum.Core.NetworkModel.Language       ( NetworkModel )

data NetworkingL a where
  Connect :: D.ConnectionConfig -> NetworkingL (Maybe D.Connection)
  EvalNetwork :: Eff NetworkModel a -> NetworkingL a

makeFreer ''NetworkingL