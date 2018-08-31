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

import           Enecuum.Core.NetworkModel.Language       ( NetworkSendingL, NetworkListeningL, NetworkSyncL )
import           Enecuum.Framework.Networking.Language    ( NetworkingL )
import           Enecuum.Framework.Node.Language          ( NodeL )
import qualified Enecuum.Framework.Domain      as D

type NodeModel =
  '[ NetworkSendingL
   , NetworkListeningL
   , NetworkSyncL
   , NetworkingL
   , NodeL
   ]

type Handler = (Eff NodeModel (), Maybe BS.ByteString)
type HandlersF = Handler -> Handler

data NodeDefinitionL a where
  NodeTag        :: D.NodeTag -> NodeDefinitionL a
  Initialization :: Eff NodeModel a -> NodeDefinitionL a
  Serving        :: HandlersF -> NodeDefinitionL D.ServerHandle

type NodeDefinitionModel =
  '[ NodeDefinitionL
   ]

makeFreer ''NodeDefinitionL

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
