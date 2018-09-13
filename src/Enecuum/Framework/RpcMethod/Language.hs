{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}

module Enecuum.Framework.RpcMethod.Language where

import           Enecuum.Prelude

import           Enecuum.Framework.Node.Language          ( NodeModel )

-- | Rpc server description language.
data RpcMethod where
  -- | Set rpc method to list.
  RpcMethod :: String -> Eff NodeModel a -> RpcMethod

