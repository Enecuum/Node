
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE DeriveAnyClass      #-}

module Enecuum.Tests.Integration.DatabaseSpec where

import qualified Data.Aeson as A
import qualified Data.Map as M

import           Enecuum.Prelude
import           Enecuum.Domain as D

import           Test.Hspec


readWriteScenario :: Database -> D.StringHash -> D.StringHash -> L.NodeL ()
readWriteScenario db hash1 hash2 = 
    L.withDatabase db $ do
        eTrans      <- L.findValue @Transaction hash1
        eMicroblock <- L.findValue @Microblock hash1
        case (eTrans, eMicroblock) of
            (Left D.InvalidType, Left D.InvalidType) -> L.logInfo $ "Object of invalid type found for " <> show hash1
            (Left D.KeyNotFound, _)                  -> L.logInfo $ "Key not found: " <> show hash1
            (Right trans, _)                         -> L.logInfo $ "Transaction got for " <> show hash1 <> ": " <> show trans
            (_, Right mBlock)                        -> L.logInfo $ "Microblock got for " <> show hash1 <> ": " <> show mBlock


traverseBackwardScenario :: Database -> D.KBlock -> L.NodeL ()
traverseBackwardScenario db kBlock = 
    L.withDatabase @KBlock db $ do
        eTrans <- L.findValue hash1
        eMicroblock <- L.findValue @Microblock hash1
        case (eTrans, eMicroblock) of
            (Left D.InvalidType, Left D.InvalidType) -> L.logInfo $ "Object of invalid type found for " <> show hash1
            (Left D.KeyNotFound, _)                  -> L.logInfo $ "Key not found: " <> show hash1
            (Right trans, _)                         -> L.logInfo $ "Transaction got for " <> show hash1 <> ": " <> show trans
            (_, Right mBlock)                        -> L.logInfo $ "Microblock got for " <> show hash1 <> ": " <> show mBlock




spec :: Spec
spec = describe "Database support tests" $ do
    it "Test 1" $ fail "failed"
    it "Test 2" $ fail "failed"
