{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}

module Enecuum.Framework.NodeDefinition.Language where

import           Eff.TH     (makeFreer )
import           Eff        (Eff, Member, send)
import qualified Data.Aeson as A
import           Data.Aeson (FromJSON)
import qualified Data.ByteString.Lazy          as BS

import Enecuum.Core.NetworkModel.Language (NetworkModelL)
import Enecuum.Framework.Networking.Language (NetworkingL)
import Enecuum.Framework.Node.Language (NodeL)
import qualified Enecuum.Framework.Domain as D

type NodeLanguage effs =
  ( Member NetworkModelL effs
  , Member NetworkingL effs
  , Member NodeL effs
  )

type Handler effs = (Eff effs (), Maybe BS.ByteString)
type HandlersF effs = Handler effs -> Handler effs

-- data NodeDefinitionL a where
--   Initialization :: (NodeLanguage effs => Eff effs a) -> NodeDefinitionL a
--   Serving        :: (NodeLanguage effs => HandlersF effs) -> NodeDefinitionL D.ServerHandle

data NodeDefinitionL a where
  Initialization :: Eff effs a -> NodeDefinitionL a
  Serving        :: HandlersF effs -> NodeDefinitionL D.ServerHandle

-- initialization
--   :: forall effs1 effs2
--    . ( NodeLanguage effs1
--      , Member NodeDefinitionL effs2
--      )
--   => Eff effs1 D.NodeID
--   -> Eff effs2 D.NodeID
-- initialization = send . Initialization
--
-- serving
--   :: forall effs1 effs2
--   . ( NodeLanguage effs1
--     , Member NodeDefinitionL effs2
--     )
--   => HandlersF effs1
--   -> Eff effs2 D.ServerHandle
-- serving = send . Serving

initialization
  :: forall effs
   . ( NodeLanguage effs
     , Member NodeDefinitionL effs
     )
  => Eff effs D.NodeID
  -> Eff effs D.NodeID
initialization = send . Initialization

serving
  :: forall effs
  . ( NodeLanguage effs
    , Member NodeDefinitionL effs
    )
  => HandlersF effs
  -> Eff effs D.ServerHandle
serving = send . Serving

serveRequest
  :: forall req effs
   . (FromJSON req, NodeLanguage effs)
  => (req -> Eff effs ())
  -> (Eff effs (), Maybe BS.ByteString)
  -> (Eff effs (), Maybe BS.ByteString)
serveRequest handler (handled, Just rawReq) = case A.decode rawReq of
  Just req -> (handled >> handler req, Nothing)
  Nothing  -> (handled, Just rawReq)
serveRequest handler (handled, Nothing) = (handled, Nothing)
