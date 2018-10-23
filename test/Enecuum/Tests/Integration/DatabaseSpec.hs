
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


-- readWriteScenario :: Database -> D.StringHash -> D.StringHash -> L.NodeL ()
-- readWriteScenario db hash1 hash2 = 
--     L.withDatabase db $ do
--         eTrans      <- L.findValue @Transaction hash1
--         eMicroblock <- L.findValue @Microblock hash1
--         case (eTrans, eMicroblock) of
--             (Left (D.InvalidType _), (Left D.InvalidType _)) -> L.logInfo $ "Object of invalid type found for " <> show hash1
--             (Left (D.KeyNotFound k), _)                      -> L.logInfo $ "Key not found: " <> k
--             (Right trans, _)                                 -> L.logInfo $ "Transaction got for " <> show hash1 <> ": " <> show trans
--             (_, Right mBlock)                                -> L.logInfo $ "Microblock got for " <> show hash1 <> ": " <> show mBlock


-- traverseBackwardScenario :: Database -> D.KBlock -> L.NodeL ()
-- traverseBackwardScenario db kBlock = 
--     L.withDatabase (db :: KBlock) $ do
--         eBlock <- L.find kBlock
--         case eBlock of
--             Left (D.InvalidType _) -> L.logInfo "Object of invalid type found."
--             Left (D.NotFound b   ) -> L.logInfo $ "Object not found: " <> b
--             Right (dbKey, dbItem)  -> do
--                 L.logInfo $ "Object found: " <> show kBlock
--                 pure ()

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

        -- -- index: hash -> <idx>
        -- let idx = [ ("u2Z/FYLb0IMEAGB2BuuMTaqLqzVDLrwnAwOMU3ZghZY=", 1)
        --           , ("kBJ+EP7GVRRUTTMIajqCstccuEknuP5RuCWtxRoUDw0=", 2)
        --           , ("6p0OqA4+ypM0byKcZfI97JmnPoufyVn0xYkw1F3Fj7I=", 3)
        --           ]

        -- let kblocks = [ "00000004z9ADFAWehl6XGW2/N+2keOgNR921st3oPSVxv08hTY="
        --               , "0000001u2Z/FYLb0IMEAGB2BuuMTaqLqzVDLrwnAwOMU3ZghZY="
        --               , "0000002kBJ+EP7GVRRUTTMIajqCstccuEknuP5RuCWtxRoUDw0="
        --               , "00000036p0OqA4+ypM0byKcZfI97JmnPoufyVn0xYkw1F3Fj7I="
        --               ]

        -- print $ List.sort l


{-
    kBlocks (idx|0 -> prev_hash, idx|1 -> kBlock data)
    ------------------------------------------------------------
    0000000|0       AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=
    0000000|1       {number:0, nonce: 0, solver: 1}

    0000001|0       4z9ADFAWehl6XGW2/N+2keOgNR921st3oPSVxv08hTY=
    0000001|1       {number:1, nonce: 100, solver: 2}

    0000002|0       u2Z/FYLb0IMEAGB2BuuMTaqLqzVDLrwnAwOMU3ZghZY=
    0000002|1       {number:2, nonce: 200, solver: 1}
    
    =================================================================

    kBlocks_meta (hash -> meta)
    meta: (idx, additional info)
    -----------------------------------------------------------------
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=        (AAAAAAA, "")
    4z9ADFAWehl6XGW2/N+2keOgNR921st3oPSVxv08hTY=        (0, "")
    u2Z/FYLb0IMEAGB2BuuMTaqLqzVDLrwnAwOMU3ZghZY=        (1, "")
    kBJ+EP7GVRRUTTMIajqCstccuEknuP5RuCWtxRoUDw0=        (2, "")
    
    =================================================================

    mBlocks (idx -> mBlock data)
    --------------------------------------------------------------------
    0000000       {kBlockIdx: 0, publisher: 1, signature: <signature>}
    0000001       {kBlockIdx: 1, publisher: 2, signature: <signature>}
    0000002       {kBlockIdx: 2, publisher: 1, signature: <signature>}

    ===================================================================

    mBlocks_meta (hash -> meta)
    --------------------------------------------------------------------
    <mblock0_hash>         (0, "")
    <mblock1_hash>         (1, "")
    <mblock2_hash>         (2, "")

    =============================================================================

    txs (mBlock idx -> transaction data)
    --------------------------------------------------------------------
    000000000         {owner: 1, receiver: 2, amount: 100: signature: <signature>}
    000000001         {owner: 2, receiver: 3, amount: 500: signature: <signature>}
    000000002         {owner: 1, receiver: 3, amount: 101: signature: <signature>}

    ==============================================================================

    txs_meta (hash -> meta)
    --------------------------------------------------------------------
    <trans9_hash>     (0, "")
    <trans1_hash>     (1, "")
    <trans2_hash>     (2, "")

    ==============================================================================

    actors (solver/publisher idx -> solver / publisher id)
    --------------------------------------------------------------------
    1   <solver 1 id>
    2   <solver 2 id>
    3   <publisher 1 id>
    4   <publisher 2 id>
    5   <publisher 3 id>

    ===================================================================

    ledger (wallet_id -> amount)


    ===================================================================


-}
