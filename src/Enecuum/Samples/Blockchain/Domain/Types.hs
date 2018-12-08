{-# LANGUAGE DeriveAnyClass #-}
module Enecuum.Samples.Blockchain.Domain.Types where 

import Enecuum.Prelude
import qualified Data.Serialize          as S


data Currency = ENQ | ETH | DASH | BTC deriving (Ord, Eq, Read, Show, Generic, ToJSON, FromJSON, S.Serialize)    
type Time      = Int -- UnixTimestamp    
type Amount = Integer
