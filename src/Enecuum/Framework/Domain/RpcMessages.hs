module Enecuum.Framework.Domain.RpcMessages where

import           Enecuum.Prelude
import           Data.Aeson as A

data RpcRequest  = RpcRequest  Text A.Value Int

data RpcResponse
    = RpcResponseResult A.Value Int
    | RpcResponseError A.Value Int


instance FromJSON RpcRequest where
    parseJSON (Object o) = RpcRequest
        <$> o .: "method"
        <*> o .: "params"
        <*> o .: "id"
    parseJSON _ = error ""

instance ToJSON RpcRequest where
    toJSON (RpcRequest method val requesId) = object [
        "method" A..= method,
        "params" A..= val,
        "id"     A..= requesId 
        ]

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