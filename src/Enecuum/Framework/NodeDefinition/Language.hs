{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}

module Enecuum.Framework.NodeDefinition.Language where

import           Enecuum.Prelude

import qualified Data.Aeson                    as A

import           Enecuum.Framework.Node.Language          ( NodeModel )
import qualified Enecuum.Framework.Domain      as D


type Handler = (Eff NodeModel (), Maybe D.RawData)
type HandlersF = Handler -> Handler

data NodeDefinitionL a where
  NodeTag        :: D.NodeTag -> NodeDefinitionL ()
  Initialization :: Eff NodeModel a -> NodeDefinitionL a
  Serving        :: HandlersF -> NodeDefinitionL ()

makeFreer ''NodeDefinitionL

-- Raw idea of RPC description. Will be reworked.
serve
  :: FromJSON req
  => (req -> Eff NodeModel ())
  -> (Eff NodeModel (), Maybe D.RawData)
  -> (Eff NodeModel (), Maybe D.RawData)
serve handler (handled, Just rawReq) = case A.decode rawReq of
  Just req -> (handled >> handler req, Nothing)
  Nothing  -> (handled, Just rawReq)
serve _ (handled, Nothing) = (handled, Nothing)