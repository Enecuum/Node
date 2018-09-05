{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Enecuum.Framework.Node.Language where

import           Enecuum.Prelude

import qualified Data.ByteString.Lazy          as BS
import qualified Data.Aeson                    as A

import           Enecuum.Core.Language                    ( CoreEffects )
import           Enecuum.Framework.NetworkModel.Language  ( NetworkSendingL, NetworkListeningL, NetworkSyncL )
import           Enecuum.Framework.Networking.Language    ( NetworkingL )
import qualified Enecuum.Framework.Domain                 as D

data NodeL a where
  Dummy :: NodeL ()

makeFreer ''NodeL

type NodeModel =
  '[ NodeL
   , NetworkingL
   , NetworkSyncL
   , NetworkListeningL
   , NetworkSendingL
   ]
  ++ CoreEffects


type Handler = (Eff NodeModel (), Maybe D.RawData)
type HandlersF = Handler -> Handler

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
