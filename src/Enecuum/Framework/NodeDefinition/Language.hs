{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}

module Enecuum.Framework.NodeDefinition.Language where

import           Eff.TH                                   ( makeFreer )
import           Eff                                      ( Eff
                                                          , Member
                                                          , send
                                                          )
import qualified Data.Aeson                    as A
import           Data.Aeson                               ( FromJSON )
import qualified Data.ByteString.Lazy          as BS

import           Enecuum.Core.NetworkModel.Language       ( NetworkModelL )
import           Enecuum.Framework.Networking.Language    ( NetworkingL )
import           Enecuum.Framework.Node.Language          ( NodeL )
import qualified Enecuum.Framework.Domain      as D

type LanguageEffs =
  '[ NetworkModelL
   , NetworkingL
   , NodeL
   ]

type Handler = (Eff LanguageEffs (), Maybe BS.ByteString)
type HandlersF = Handler -> Handler

data NodeDefinitionL a where
  Initialization :: Eff LanguageEffs a -> NodeDefinitionL a
  Serving        :: HandlersF -> NodeDefinitionL D.ServerHandle

initialization
  :: forall effs
   . Member NodeDefinitionL effs
  => Eff LanguageEffs D.NodeID 
  -> Eff effs D.NodeID
initialization = send . Initialization

serving
  :: forall effs
   . (Member NodeDefinitionL effs)
  => HandlersF
  -> Eff effs D.ServerHandle
serving = send . Serving

serveRequest
  :: forall req effs
   . FromJSON req
  => (req -> Eff effs ())
  -> (Eff effs (), Maybe BS.ByteString)
  -> (Eff effs (), Maybe BS.ByteString)
serveRequest handler (handled, Just rawReq) = case A.decode rawReq of
  Just req -> (handled >> handler req, Nothing)
  Nothing  -> (handled, Just rawReq)
serveRequest _ (handled, Nothing) = (handled, Nothing)
