{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Enecuum.Framework.Node.Language where

import           Eff.TH    ( makeFreer )
import           Eff.State                                (State)
import           Eff.SafeIO                               (SIO)
import           Eff.Exc                                  (Exc)
import           Control.Exception                        (SomeException)

import           Enecuum.Core.NetworkModel.Language       ( NetworkSendingL, NetworkListeningL, NetworkSyncL )
import           Enecuum.Framework.Networking.Language    ( NetworkingL )
import qualified Enecuum.Framework.Domain as D

import           Enecuum.RuntimeTmp

data NodeL a where
  Dummy :: NodeL ()

makeFreer ''NodeL


type NodeModel =
  '[ NodeL
   , NetworkingL
   , NetworkSyncL
   , NetworkListeningL
   , NetworkSendingL
   , State RuntimeSt
   , SIO
   , Exc SomeException
   ]
