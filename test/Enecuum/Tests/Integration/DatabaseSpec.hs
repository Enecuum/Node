
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE DeriveAnyClass      #-}

module Enecuum.Tests.Integration.DatabaseSpec where

import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.List as List

import           Enecuum.Prelude
import           Enecuum.Domain as D

import           Test.Hspec


-- readWriteScenario :: Database  -> D.StringHash -> D.StringHash -> L.NodeL ()
-- readWriteScenario db hash1 hash2 = 
--     L.withDatabase db $ do
--         eTrans      <- L.findValue @Transaction hash1
--         eMicroblock <- L.findValue @Microblock hash1
--         case (eTrans, eMicroblock) of
--             (Left (D.InvalidType _), (Left D.InvalidType _)) -> L.logInfo $ "Object of invalid type found for " <> show hash1
--             (Left (D.KeyNotFound k), _)                      -> L.logInfo $ "Key not found: " <> k
--             (Right trans, _)                                 -> L.logInfo $ "Transaction got for " <> show hash1 <> ": " <> show trans
--             (_, Right mBlock)                                -> L.logInfo $ "Microblock got for " <> show hash1 <> ": " <> show mBlock

-- withDatabase :: D.Database DBEntity -> L.DatabaseL a -> L.NodeL a


traverseBackwardScenario :: D.Database KBlock -> D.KBlock -> L.NodeL ()
traverseBackwardScenario db kBlock = 
    L.withDatabase db $ do
        eBlock <- L.find kBlock
        case eBlock of
            Left (D.InvalidType _) -> L.logInfo "Object of invalid type found."
            Left (D.NotFound b   ) -> L.logInfo $ "Object not found: " <> b
            Right (dbKey, dbItem)  -> do
                L.logInfo $ "Object found: " <> show kBlock
                pure ()

kBlock1 = D.KBlock
    { _time      = 0
    , _prevHash  = D.genesisHash
    , _number    = 1
    , _nonce     = 0
    , _solver    = D.genesisHash
    }

kBlock2 = D.KBlock
    { _time      = 0
    , _prevHash  = toHash kBlock1
    , _number    = 2
    , _nonce     = 0
    , _solver    = D.genesisHash
    }

kBlock3 = D.KBlock
    { _time      = 0
    , _prevHash  = toHash kBlock2
    , _number    = 3
    , _nonce     = 0
    , _solver    = D.genesisHash
    }


spec :: Spec
spec = describe "Database support tests" $ do
    it "Test 1" $ "failed" `shouldBe` "1"
    it "Test 2" $ do
        print $ D.genesisHash
        print $ toHash kBlock1
        print $ toHash kBlock2
        print $ toHash kBlock3

        "failed" `shouldBe` "1"
