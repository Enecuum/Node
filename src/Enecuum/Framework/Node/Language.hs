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


-- Raw idea of RPC description. Will be reworked.

type Handler = (Eff NodeModel (Maybe D.RawData), D.RawData)
type HandlersF = Handler -> Handler

tryHandler
  :: D.RpcMethod () req resp
  => FromJSON req
  => ToJSON resp
  => (req -> Eff NodeModel resp)
  -> D.RawData
  -> Eff NodeModel (Maybe D.RawData)
tryHandler handler rawReq = case A.decode rawReq of
  Nothing -> pure Nothing
  Just req -> do
    resp <- handler req
    pure $ Just $ A.encode resp

serve
  :: D.RpcMethod () req resp
  => FromJSON req
  => ToJSON resp
  => (req -> Eff NodeModel resp)
  -> (Eff NodeModel (Maybe D.RawData), D.RawData)
  -> (Eff NodeModel (Maybe D.RawData), D.RawData)
serve handler (prevHandled, rawReq) = (newHandled, rawReq)
  where
    newHandled = prevHandled >>= \case
      Nothing      -> tryHandler handler rawReq
      Just rawResp -> pure $ Just rawResp
