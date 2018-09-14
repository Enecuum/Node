{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Enecuum.Framework.Node.Language where

import           Enecuum.Prelude

import qualified Data.Aeson                               as A
import           Eff                                      ( send )

import           Enecuum.Core.Language                    ( CoreEffects, HGraphModel )
import           Enecuum.Core.HGraph.Language             ( W )
import           Enecuum.Framework.NetworkModel.Language  ( NetworkSendingL, NetworkListeningL, NetworkSyncL )
import           Enecuum.Framework.Networking.Language    ( NetworkingL )
import qualified Enecuum.Framework.Domain                 as D

import qualified Enecuum.Framework.Domain.Types           as T

-- | Node language.
data NodeL a where
  EvalGraphAction :: Eff T.LGraphModel a -> NodeL a

-- | Node model langauges. These langauges should be used in the node scripts.
-- With these languages, nodes can interact through the network,
-- work with internal state.
type NodeModel =
  '[ NodeL
   , NetworkingL
   , NetworkSyncL
   , NetworkListeningL
   , NetworkSendingL
   ]
  ++ CoreEffects

evalGraphAction :: Eff T.LGraphModel a -> Eff NodeModel a
evalGraphAction = send . EvalGraphAction

-- Raw idea of RPC description. Will be reworked.

-- | Handler is a function which processes a particular response
-- if this response is what RawData contains.
type Handler = (Eff NodeModel (Maybe D.RawData), D.RawData)

-- | HandlersF is a function holding stack of handlers which are handling
-- different requests.
type HandlersF = Handler -> Handler

-- | Tries to decode a request into a request the handler accepts.
-- On success, calls the handler and returns Just result.
-- On failure, returns Nothing.
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

-- | Allows to specify a stack of handlers for different RPC requests.
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
