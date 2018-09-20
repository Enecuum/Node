{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Framework.TestData.RPC where

import Enecuum.Prelude

import qualified Data.Aeson                       as A

import qualified Enecuum.Domain                   as D

-- Types for RPC requests.

data GetHashIDRequest = GetHashIDRequest
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype GetHashIDResponse = GetHashIDResponse Text
  deriving (Show, Eq, Generic, Newtype, ToJSON, FromJSON)



data ValidationRequest = ValidRequest | InvalidRequest
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype ValidationResponse = ValidationResponse (Either [Text] Text)
  deriving (Show, Eq, Generic, Newtype, ToJSON, FromJSON)

instance D.RpcMethod () ValidationRequest ValidationResponse where
  toRpcRequest _ = D.RpcRequest . A.encode
  fromRpcResponse _ (D.RpcResponse raw)
    = maybe (Left $ "No parse of validation resp" +|| raw ||+ "") Right $ A.decode raw

newtype HelloRequest1  = HelloRequest1 { helloMessage :: Text }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype HelloResponse1 = HelloResponse1 { ackMessage :: Text }
  deriving (Show, Eq, Generic, Newtype, ToJSON, FromJSON)



newtype HelloRequest2  = HelloRequest2 { helloMessage :: Text }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype HelloResponse2 = HelloResponse2 { ackMessage :: Text }
  deriving (Show, Eq, Generic, Newtype, ToJSON, FromJSON)


-------------------------------------------------------------------

data GetBalanceRequest = GetBalanceRequest
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype GetBalanceResponse = GetBalanceResponse { balance :: Int }
  deriving (Show, Eq, Generic, Newtype, ToJSON, FromJSON)

newtype BalanceChangeRequest = BalanceChangeRequest Int
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype BalanceChangeResponse = BalanceChangeResponse { balance :: Maybe Int }
  deriving (Show, Eq, Generic, Newtype, ToJSON, FromJSON)

