{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}

module Enecuum.Framework.RpcMethod.Language where

import           Enecuum.Prelude
import           Data.Aeson as A

import           Enecuum.Framework.Node.Language          ( NodeModel )


-- | Rpc server description language.
data RpcMethodL a where
  -- | Set rpc method to list.
  RpcMethod :: Text -> RpcMethod -> (() -> a)  -> RpcMethodL a

instance Functor RpcMethodL where
  fmap g (RpcMethod text method next) = RpcMethod text method (g . next)

type RpcMethod = A.Value -> Int -> NodeModel RpcResponse

rpcMethod :: Text -> RpcMethod -> Free RpcMethodL ()
rpcMethod text method = liftF (RpcMethod text method id)

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
  
