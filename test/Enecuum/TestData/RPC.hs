{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.TestData.RPC where

import Enecuum.Prelude

import qualified Enecuum.Language as L

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

-- RPC handlers.

acceptHello1 :: HelloRequest1 -> L.NodeL HelloResponse1
acceptHello1 (HelloRequest1 msg) = pure $ HelloResponse1 $ "Hello, dear. " +| msg |+ ""

acceptHello2 :: HelloRequest2 -> L.NodeL HelloResponse2
acceptHello2 (HelloRequest2 msg) = pure $ HelloResponse2 $ "Hello, dear2. " +| msg |+ ""

acceptGetHashId :: GetHashIDRequest -> L.NodeL GetHashIDResponse
acceptGetHashId GetHashIDRequest = pure $ GetHashIDResponse "1"
