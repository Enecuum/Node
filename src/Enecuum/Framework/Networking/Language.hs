{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Enecuum.Framework.Networking.Language where

import           Eff.TH    ( makeFreer )
import           Eff       ( Eff, Member, send)

import qualified Enecuum.Framework.Domain as D

import           Enecuum.Core.NetworkModel.Language (NetworkModelL)

data NetworkingL a where
  Connect :: D.ConnectionConfig -> NetworkingL (Maybe D.Connection)
  WithNetwork :: (Member NetworkModelL effs) => Eff effs a -> NetworkingL a

withNetwork
  :: forall effs1 effs2 a
   . ( Member NetworkModelL effs1
     , Member NetworkingL effs2
     )
  => Eff effs1 a
  -> Eff effs2 a
withNetwork = send . WithNetwork

withConnection
  :: Member NetworkingL effs
  => D.ConnectionConfig
  -> D.NodeConfig
  -> (D.Connection -> Eff effs a)
  -> Eff effs a
withConnection = undefined
