module Enecuum.Framework.Domain.RPC where

import           Enecuum.Prelude
import           Data.Aeson as A
import           Data.Text as T
import           Data.Typeable

data RpcRequest  = RpcRequest Text A.Value Int
    deriving (Show)

data RpcResponse
    = RpcResponseResult A.Value Int
    | RpcResponseError A.Value Int
    deriving (Show)

toRpcRequest :: (Typeable a, ToJSON a) => a -> RpcRequest
toRpcRequest a = RpcRequest (T.pack . show . typeOf $ a) (toJSON a) 0

class Content a where
    content :: a -> A.Value

instance Content RpcRequest where
    content (RpcRequest _ a _) = a

instance Content RpcResponse where
    content (RpcResponseResult a _) = a
    content (RpcResponseError a _) = a

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
