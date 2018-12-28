{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PackageImports         #-}
{-# LANGUAGE TemplateHaskell        #-}

module Enecuum.Tests.Integration.DatabaseSpec where

import           Enecuum.Prelude

import qualified Enecuum.Domain                      as D
import qualified Enecuum.Language                    as L
import           Enecuum.Samples.Blockchain.DB
import qualified Enecuum.Samples.Blockchain.Domain   as D
import qualified Enecuum.Samples.Blockchain.Language as L
import qualified Enecuum.Samples.Blockchain.Lens     as Lens
import           Enecuum.Testing.Integrational
import           Enecuum.Tests.Helpers
import           Enecuum.Testing.Wrappers
import           Test.Hspec

data NodeData = NodeData
    { _kBlocksDB     :: D.Storage KBlocksDB
    , _kBlocksMetaDB :: D.Storage KBlocksMetaDB
    }

makeFieldsNoPrefix ''NodeData

putKBlockMetaNode :: D.KBlock -> D.DBConfig KBlocksMetaDB -> L.NodeDefinitionL (Either D.DBError ())
putKBlockMetaNode kBlock cfg = do
    let k = D.toDBKey   @KBlockMetaEntity kBlock
    let v = D.toDBValue @KBlockMetaEntity kBlock
    eDB <- L.scenario $ L.initDatabase cfg
    case eDB of
        Left err -> pure $ Left err
        Right db -> L.scenario
            $ L.withDatabase db
            $ L.putEntity k v

getKBlockMetaNode :: D.DBKey KBlockMetaEntity -> D.DBConfig KBlocksMetaDB -> L.NodeDefinitionL (Either D.DBError (D.DBValue KBlockMetaEntity))
getKBlockMetaNode k cfg = do
    eDB <- L.scenario $ L.initDatabase cfg
    case eDB of
        Left err -> pure $ Left err
        Right db -> L.scenario $ L.withDatabase db $ L.getValue k

putGetKBlockMetaNode :: FilePath -> L.NodeDefinitionL (Either D.DBError Bool)
putGetKBlockMetaNode dbPath = do
    let dbOpts = D.DBOptions
            { D._createIfMissing = True
            , D._errorIfExists   = True
            }
    let cfg :: D.DBConfig KBlocksMetaDB = D.DBConfig dbPath dbOpts
    eDB <- L.scenario $ L.initDatabase cfg
    case eDB of
        Left err -> pure $ Left err
        Right db -> L.scenario $ L.withDatabase db $ do
            eRes <- L.putEntity kBlock1MetaKey kBlock1MetaValue
            case eRes of
                Left err -> pure $ Left err
                Right _  -> do
                    eVal <- L.getValue kBlock1MetaKey
                    pure $ eVal >>= (\val2 -> Right (kBlock1MetaValue == val2))

kBlock1 :: D.KBlock
kBlock1 = D.KBlock
    { D._time      = 0
    , D._prevHash  = D.genesisHash
    , D._number    = 1
    , D._nonce     = 0
    , D._solver    = D.genesisHash
    }

kBlock2 :: D.KBlock
kBlock2 = D.KBlock
    { D._time      = 1
    , D._prevHash  = D.toHash kBlock1
    , D._number    = 2
    , D._nonce     = 2
    , D._solver    = D.genesisHash
    }

kBlock3 :: D.KBlock
kBlock3 = D.KBlock
    { D._time      = 3
    , D._prevHash  = D.toHash kBlock2
    , D._number    = 3
    , D._nonce     = 3
    , D._solver    = D.genesisHash
    }

kBlock1MetaKey :: D.DBKey KBlockMetaEntity
kBlock1MetaKey = D.toDBKey kBlock1

kBlock1MetaValue :: D.DBValue KBlockMetaEntity
kBlock1MetaValue = D.toDBValue kBlock1

kBlock2MetaKey :: D.DBKey KBlockMetaEntity
kBlock2MetaKey = D.toDBKey kBlock2

kBlock2MetaValue :: D.DBValue KBlockMetaEntity
kBlock2MetaValue = D.toDBValue kBlock2

kBlock3MetaKey :: D.DBKey KBlockMetaEntity
kBlock3MetaKey = D.toDBKey kBlock3

kBlock3MetaValue :: D.DBValue KBlockMetaEntity
kBlock3MetaValue = D.toDBValue kBlock3

dbInitNode :: D.DBConfig db -> L.NodeDefinitionL (D.DBResult ())
dbInitNode cfg = do
    eDb <- L.scenario $ L.initDatabase cfg
    pure $ eDb >> Right ()

spec :: Spec
spec = stableTest $ fastTest $ describe "Database functional tests" $ do
    dbPath <- runIO $ mkDbPath "test.db"
    let cfg1 = D.DBConfig dbPath $ D.defaultDbOptions
                { D._createIfMissing = True
                , D._errorIfExists   = False
                }
    let cfg2 = D.DBConfig dbPath $ D.defaultDbOptions
                { D._createIfMissing = True
                , D._errorIfExists   = True
                }

    describe "DB Entities tests" $ do
        it "ToDBKey test" $
            kBlock1MetaKey   `shouldBe` KBlockMetaKey (kBlock1 ^. Lens.prevHash)

        it "ToDBValue test" $
            kBlock1MetaValue `shouldBe` KBlockMetaValue 1

        it "RawDBEntity test" $
            D.toRawDBKey @KBlocksMetaDB kBlock1MetaKey `shouldBe` D.fromStringHash (kBlock1 ^. Lens.prevHash)

        it "Parse RawDBValue test" $ do
            let dbValueRaw = D.toRawDBValue @KBlocksMetaDB kBlock1MetaValue
            D.fromRawDBValue @KBlocksMetaDB dbValueRaw `shouldBe` Just kBlock1MetaValue

        it "Different objects => different keys and values" $ do
            kBlock1MetaKey   `shouldNotBe` kBlock2MetaKey
            kBlock1MetaValue `shouldNotBe` kBlock2MetaValue
            D.toRawDBKey   @KBlocksMetaDB kBlock1MetaKey   `shouldNotBe` D.toRawDBKey   @KBlocksMetaDB kBlock2MetaKey
            D.toRawDBValue @KBlocksMetaDB kBlock1MetaValue `shouldNotBe` D.toRawDBValue @KBlocksMetaDB kBlock2MetaValue

    describe "Database creation tests" $ do
        it "DB is missing, create, errorIfExists False, no errors expected" $ withDbAbsence dbPath $ do
            eRes <- evalNode $ dbInitNode cfg1
            eRes `shouldBe` Right ()

        it "DB is missing, create, errorIfExists True, no errors expected" $ withDbAbsence dbPath $ do
            eRes <- evalNode $ dbInitNode cfg2
            eRes `shouldBe` Right ()

        it "DB is present, create, errorIfExists False, no errors expected" $ withDbPresence dbPath $ do
            eRes <- evalNode $ dbInitNode cfg1
            eRes `shouldBe` Right ()

        it "DB is present, create, errorIfExists False, errors expected" $ withDbPresence dbPath $ do
            eRes <- evalNode $ dbInitNode cfg2
            eRes `shouldBe` Left (D.DBError D.SystemError ("user error (open: Invalid argument: " +| dbPath |+ ": exists (error_if_exists is true))"))

    describe "Database usage tests" $ do
        it "Write and Read KBlock Meta" $ withDbAbsence dbPath $ do
            eRes <- evalNode $ putGetKBlockMetaNode dbPath
            eRes `shouldBe` Right True

        it "Write and Read KBlock1 Meta in separate runs" $ withDbAbsence dbPath $ do
            eInitialized <- evalNode $ dbInitNode cfg1
            eInitialized `shouldBe` Right ()

            eStoreResult <- evalNode $ putKBlockMetaNode kBlock1 cfg1
            eStoreResult `shouldBe` Right ()

            eValue <- evalNode $ getKBlockMetaNode kBlock1MetaKey cfg1
            eValue `shouldBe` Right kBlock1MetaValue

        it "Write and Read KBlock2 Meta in separate runs" $ withDbAbsence dbPath $ do
            eInitialized <- evalNode $ dbInitNode cfg1
            eInitialized `shouldBe` Right ()

            eStoreResult <- evalNode $ putKBlockMetaNode kBlock2 cfg1
            eStoreResult `shouldBe` Right ()

            eValue <- evalNode $ getKBlockMetaNode kBlock2MetaKey cfg1
            eValue `shouldBe` Right kBlock2MetaValue

        it "Write and Read KBlock3 Meta in separate runs" $ withDbAbsence dbPath $ do
            eInitialized <- evalNode $ dbInitNode cfg1
            eInitialized `shouldBe` Right ()

            eStoreResult <- evalNode $ putKBlockMetaNode kBlock3 cfg1
            eStoreResult `shouldBe` Right ()

            eValue <- evalNode $ getKBlockMetaNode kBlock3MetaKey cfg1
            eValue `shouldBe` Right kBlock3MetaValue

        it "Read unexisting KBlock Meta" $ withDbPresence dbPath $ do
            eInitialized <- evalNode $ dbInitNode cfg1
            eInitialized `shouldBe` Right ()

            eValue <- evalNode $ getKBlockMetaNode kBlock1MetaKey cfg1
            eValue `shouldBe` Left (D.DBError D.KeyNotFound (show $ D.fromStringHash $ kBlock1 ^. Lens.prevHash))

        it "Write one, read another (unexisting) KBlock Meta" $ withDbPresence dbPath $ do
            eInitialized <- evalNode $ dbInitNode cfg1
            eInitialized `shouldBe` Right ()

            eStoreResult <- evalNode $ putKBlockMetaNode kBlock1 cfg1
            eStoreResult `shouldBe` Right ()

            eValue <- evalNode $ getKBlockMetaNode kBlock2MetaKey cfg1
            eValue `shouldBe` Left (D.DBError D.KeyNotFound (show $ D.fromStringHash $ kBlock2 ^. Lens.prevHash))

        it "Write two entities, read both" $ withDbPresence dbPath $ do
            eInitialized <- evalNode $ dbInitNode cfg1
            eInitialized `shouldBe` Right ()

            eStoreResult1 <- evalNode $ putKBlockMetaNode kBlock1 cfg1
            eStoreResult1 `shouldBe` Right ()

            eStoreResult2 <- evalNode $ putKBlockMetaNode kBlock2 cfg1
            eStoreResult2 `shouldBe` Right ()

            eValue1 <- evalNode $ getKBlockMetaNode kBlock1MetaKey cfg1
            eValue1 `shouldBe` Right kBlock1MetaValue

            eValue2 <- evalNode $ getKBlockMetaNode kBlock2MetaKey cfg1
            eValue2 `shouldBe` Right kBlock2MetaValue
