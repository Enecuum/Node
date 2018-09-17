{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}

module Enecuum.Framework.NodeDefinition.Language where

import           Enecuum.Prelude

import           Enecuum.Core.Language                    ( CoreEffects )
import           Enecuum.Framework.Node.Language          ( NodeModel, HandlersF )
import qualified Enecuum.Framework.Domain                 as D

-- TODO: it's possible to make these steps evaluating step-by-step, in order.
-- Think about if this really needed.

-- | Node description language.
-- Allows to specify what actions should be done when node starts.
data NodeDefinitionL a where
  -- | Set node tag. For example, "boot node".
  NodeTag        :: D.NodeTag -> NodeDefinitionL ()
  -- | Evaluate some node model.
  EvalNodeModel :: Eff NodeModel a -> NodeDefinitionL a
  -- | Serving of RPC requests.
  Serving        :: HandlersF -> NodeDefinitionL ()

makeFreer ''NodeDefinitionL

-- | Node definition model language.
type NodeDefinitionModel =
  '[ NodeDefinitionL
   ]
  ++ CoreEffects

-- | Runs scenario as initialization.
initialization
  :: Eff NodeModel a
  -> Eff NodeDefinitionModel a
initialization = evalNodeModel

-- | Runs scenario.
scenario
  :: Eff NodeModel a
  -> Eff NodeDefinitionModel a
scenario = evalNodeModel