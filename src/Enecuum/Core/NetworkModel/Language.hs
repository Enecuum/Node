{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Enecuum.Core.NetworkModel.Language where

import           Eff.TH                                   ( makeFreer )

import qualified Enecuum.Core.Types            as T

-- This is a raw vision of networking model. Will be updated later.

data NetworkModelL a where
  -- Unicast   :: T.NetworkCfg -> T.NetworkRequest -> NetworkModelL ()
  -- Broadcast :: T.NetworkCfg -> T.NetworkRequest -> NetworkModelL ()
  Multicast :: T.NetworkCfg -> T.NetworkRequest -> NetworkModelL ()
  FindDNSRecord :: T.NetworkCfg -> T.DNSQuery -> NetworkModelL (Maybe T.DNSResponse)

makeFreer ''NetworkModelL
