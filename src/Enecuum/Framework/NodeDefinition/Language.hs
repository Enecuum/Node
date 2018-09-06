{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}

module Enecuum.Framework.NodeDefinition.Language where

import           Enecuum.Prelude

import           Enecuum.Framework.Node.Language          ( NodeModel, HandlersF )
import qualified Enecuum.Framework.Domain                 as D

data NodeDefinitionL a where
  NodeTag        :: D.NodeTag -> NodeDefinitionL ()
  Initialization :: Eff NodeModel a -> NodeDefinitionL a
  Serving        :: HandlersF -> NodeDefinitionL ()

makeFreer ''NodeDefinitionL
