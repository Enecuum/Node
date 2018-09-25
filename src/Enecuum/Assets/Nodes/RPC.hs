{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- TODO: this is copy-paste from tests with little changes.
module Enecuum.Assets.Nodes.RPC where

import Enecuum.Prelude

-- Types for RPC requests.

data GetHashIDRequest = GetHashIDRequest
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype GetHashIDResponse = GetHashIDResponse Text
  deriving (Show, Eq, Generic, Newtype, ToJSON, FromJSON)


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

-------------------------------------------------------------------

data GetChainLengthRequest = GetChainLengthRequest
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype GetChainLengthResponse = GetChainLengthResponse { chainLength :: Int }
  deriving (Show, Eq, Generic, Newtype, ToJSON, FromJSON)
