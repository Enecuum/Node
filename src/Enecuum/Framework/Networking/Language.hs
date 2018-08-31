{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Enecuum.Framework.Networking.Language where

import           Eff.TH                                   ( makeFreer )
import           Eff                                      ( Eff
                                                          , Member
                                                          , send
                                                          )

import qualified Enecuum.Framework.Domain      as D

import           Enecuum.Core.NetworkModel.Language       ( NetworkModel )

data NetworkingL a where
  Connect :: D.ConnectionConfig -> NetworkingL (Maybe D.Connection)
  -- WithNetwork :: Eff NetworkModel a -> NetworkingL a

-- withNetwork
--   :: forall effs1 effs2 a
--    . (Member NetworkModel effs1, Member NetworkingL effs2)
--   => Eff effs1 a
--   -> Eff effs2 a
-- withNetwork = send . WithNetwork

-- withConnection
--   :: Member NetworkingL effs
--   => D.ConnectionConfig
--   -> D.NodeConfig
--   -> (D.Connection -> Eff effs a)
--   -> Eff effs a
-- withConnection = error "withConnection not implemented."
