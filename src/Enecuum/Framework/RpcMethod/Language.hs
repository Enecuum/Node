{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Enecuum.Framework.RpcMethod.Language where

import           Enecuum.Prelude
import           Data.Aeson as A
import           Enecuum.Core.Language                    ( CoreEffects )

import           Enecuum.Framework.Node.Language          ( NodeModel )

-- | Rpc server description language.
data RpcMethodL a where
  -- | Set rpc method to list.
  RpcMethod :: Text -> RpcMethod -> RpcMethodL ()

type RpcMethod = A.Value -> Int -> Eff NodeModel RpcResponse


data RpcResponse
  = RpcResponseResult A.Value Int
  | RpcResponseError A.Value Int

instance ToJSON RpcResponse where
  toJSON (RpcResponseResult val requesId) = object [
      "result" A..= val,
      "id"     A..= requesId 
    ]
  toJSON (RpcResponseError val requesId) = object [
      "error"  A..= val,
      "id"     A..= requesId 
    ]

instance FromJSON RpcResponse where
  parseJSON (Object a)
    = (RpcResponseResult  <$> a .: "result" <*> a .: "id")
    <|> (RpcResponseError <$> a .: "error" <*> a .: "id")
  parseJSON _ = error ""
  
makeFreer ''RpcMethodL

type RpcMethodModel =
  '[ RpcMethodL
   ]
  ++ CoreEffects
