{-# LANGUAGE DeriveAnyClass #-}
module Enecuum.Assets.Services.Pos.Types
    ( PosRole(..)
    , TrinityStage(..)
    , ShadowKey(..)
    , PosKey(..)
    ) where

import           Enecuum.Prelude

data ShadowKey = ShadowKey
data PosKey    = PosKey


data TrinityStage
    = PosInitStage
    | KBlockAccepted
    | PosVoting
    | KeyGeneration PosRole
    | KeyMade PosRole
    deriving (Show, Eq)

data PosRole = PosLeader | PosCommon deriving (Show, Eq, Generic, Serialize, ToJSON, FromJSON)