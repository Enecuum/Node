{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Enecuum.Framework.Networking.Language where

import           Eff.TH                                   ( makeFreer )
import           Eff                                      ( Eff, Member, send)
import qualified Data.ByteString.Lazy          as BS
import qualified Data.Aeson                    as A
import           Data.Aeson                               ( FromJSON )

import qualified Enecuum.Framework.Domain      as D
import           Enecuum.Core.NetworkModel.Language       ( NetworkModel )

-- This is a raw view of mid-level networking. Will change significantly.
-- Supposed to be a mid-level language hiding WebSockets and other types of connections.
-- TODO: make TCP Sockets / WebSockets / HTTP stuff more correct

data NetworkingL a where
  Connect :: D.ConnectionConfig -> NetworkingL (Maybe D.Connection) 
  EvalNetwork :: Eff NetworkModel a -> NetworkingL a

makeFreer ''NetworkingL

