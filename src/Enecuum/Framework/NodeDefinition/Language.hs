{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}

module Enecuum.Framework.NodeDefinition.Language where

import           Enecuum.Prelude

import           Enecuum.Core.Language                    ( CoreEffects )
import           Enecuum.Framework.Node.Language          ( NodeModel, HandlersF )
import qualified Enecuum.Framework.Domain                 as D

-- | Node description language.
-- Allows to specify what actions should be done when node starts.
data NodeDefinitionL a where
  -- | Set node tag. For example, "boot node".
  NodeTag        :: D.NodeTag -> NodeDefinitionL ()
  -- | Initialization of the node by evaluating of some node script.
  Initialization :: Eff NodeModel a -> NodeDefinitionL a
  -- | Serving of RPC requests.
  Serving        :: HandlersF -> NodeDefinitionL ()

makeFreer ''NodeDefinitionL

-- | Node definition model language.
type NodeDefinitionModel =
  '[ NodeDefinitionL
   ]
  ++ CoreEffects
