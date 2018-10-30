{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE PackageImports         #-}

module Enecuum.Tests.Integration.DatabaseSpec where

import           Enecuum.Prelude

import qualified Data.Aeson    as A
import           Data.Typeable (typeOf)
import           Data.Proxy    (Proxy)
import qualified Data.Map      as M
import qualified Data.List     as List
import           Data.Kind
import qualified Data.ByteString.Lazy as LBS
import           Control.Lens (makeFieldsNoPrefix)
import qualified "rocksdb-haskell" Database.RocksDB as Rocks
import qualified System.Directory as Dir
import           System.FilePath as FP ((</>))
import           Text.Printf (printf)

import           Enecuum.Interpreters                         (runNodeDefinitionL)
import qualified Enecuum.Runtime                              as R
import qualified Enecuum.Framework.NodeDefinition.Interpreter as R
import qualified Enecuum.Domain                               as D
import qualified Enecuum.Language                             as L
import qualified Enecuum.Blockchain.Lens                      as Lens
import           Enecuum.Blockchain.DB

import           Test.Hspec
import           Enecuum.Testing.Integrational

dbOpts :: D.DBOptions
dbOpts = D.DBOptions
    { D._createIfMissing = True
    , D._errorIfExists   = True
    }

data NodeData = NodeData
    { _kBlocksDB     :: D.Storage KBlocksDB
    , _kBlocksMetaDB :: D.Storage KBlocksMetaDB
    }

makeFieldsNoPrefix ''NodeData

putKBlockMetaNode :: D.DBConfig KBlocksMetaDB -> L.NodeDefinitionL (Either D.DBError ())
putKBlockMetaNode cfg = do
    eDB <- L.scenario $ L.initDatabase cfg
    case eDB of
        Left err -> pure $ Left err
        Right db -> L.scenario
            $ L.withDatabase db
            $ L.putValue kBlock1MetaKey kBlock1MetaValue

getKBlockMetaNode :: D.DBConfig KBlocksMetaDB -> L.NodeDefinitionL (Either D.DBError (D.DBValue KBlockMetaEntity))
getKBlockMetaNode cfg = do
    eDB <- L.scenario $ L.initDatabase cfg
    case eDB of
        Left err -> pure $ Left err
        Right db -> L.scenario $ L.withDatabase db $ L.getValue kBlock1MetaKey

putGetKBlockMetaNode :: FilePath -> L.NodeDefinitionL (Either D.DBError Bool)
putGetKBlockMetaNode dbPath = do
    let cfg :: D.DBConfig KBlocksMetaDB = D.DBConfig dbPath dbOpts
    eDB <- L.scenario $ L.initDatabase cfg
    case eDB of
        Left err -> pure $ Left err
        Right db -> L.scenario $ L.withDatabase db $ do
            eRes <- L.putValue kBlock1MetaKey kBlock1MetaValue
            case eRes of
                Left err -> pure $ Left err
                Right _  -> do
                    eVal <- L.getValue kBlock1MetaKey
                    pure $ eVal >>= (\val2 -> Right (kBlock1MetaValue == val2))

-- toKBlock :: D.DBValue KBlockPrevHashEntity -> D.DBValue KBlockEntity -> D.KBlock
-- toKBlock (KBlockPrevHashValue prevHash) (KBlockValue t n nc s) = D.KBlock
--     { D._time     = t
--     , D._prevHash = prevHash
--     , D._number   = n
--     , D._nonce    = nc
--     , D._solver   = s
--     }

-- getKBlock :: NodeData -> Maybe (D.DBValue KBlockMetaEntity) -> L.NodeL (Maybe D.KBlock)
-- getKBlock _         Nothing = pure Nothing
-- getKBlock nodeData (Just (KBlockMetaValue kBlockIdx)) = do
--     let key1 = toDBKey @KBlockPrevHashEntity kBlockIdx
--     let key2 = toDBKey @KBlockEntity         kBlockIdx
--     mbPrevHashValue <- L.withDatabase (nodeData ^. kBlocksDB) $ findValue key1
--     mbKBlockValue   <- L.withDatabase (nodeData ^. kBlocksDB) $ findValue key2
--     pure $ toKBlock <$> mbPrevHashValue <*> mbKBlockValue

kBlock1 :: D.KBlock
kBlock1 = D.KBlock
    { D._time      = 0
    , D._prevHash  = D.genesisHash
    , D._number    = 1
    , D._nonce     = 0
    , D._solver    = D.genesisHash
    }

kBlock1MetaKey :: D.DBKey KBlockMetaEntity
kBlock1MetaKey = D.toDBKey kBlock1

kBlock1MetaValue :: D.DBValue KBlockMetaEntity
kBlock1MetaValue = D.toDBValue kBlock1

dbInitNode :: D.DBConfig db -> L.NodeDefinitionL (D.DBResult ())
dbInitNode cfg = do
    eDb <- L.scenario $ L.initDatabase cfg
    pure $ eDb >> Right ()

mkDbPath :: FilePath -> IO FilePath
mkDbPath dbName = do
    hd <- Dir.getHomeDirectory
    pure $ hd </> ".enecuum" </> dbName

rmDb :: FilePath -> IO ()
rmDb dbPath = whenM (Dir.doesDirectoryExist dbPath) $ Dir.removeDirectoryRecursive dbPath

mkDb :: FilePath -> IO ()
mkDb dbPath = do
    rmDb dbPath
    Dir.createDirectoryIfMissing True dbPath
    -- This creates an empty DB to get correct files in the directory.
    db <- Rocks.open dbPath $ Rocks.defaultOptions { Rocks.createIfMissing = True
                                                   , Rocks.errorIfExists   = False
                                                   }
    Rocks.close db

withDbAbsence :: FilePath -> IO a -> IO ()
withDbAbsence dbPath act = do
    rmDb dbPath
    void act
    rmDb dbPath

withDbPresence :: FilePath -> IO a -> IO ()
withDbPresence dbPath act = do
    mkDb dbPath
    void act
    rmDb dbPath

spec :: Spec
spec = do
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
        it "ToDBKey test"          $ kBlock1MetaKey `shouldBe` KBlockMetaKey (D.fromStringHash $ D.toHash kBlock1)
        it "ToDBValue test"        $ kBlock1MetaValue `shouldBe` KBlockMetaValue 1
        it "GetRawDBEntity test"   $ D.getRawDBKey kBlock1MetaKey `shouldBe` D.fromStringHash (D.toHash kBlock1)
        it "Parse RawDBValue test" $ do
            let dbValueRaw = D.getRawDBValue kBlock1MetaValue
            L.parseDBValue dbValueRaw `shouldBe` Right kBlock1MetaValue

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
            eRes `shouldBe` Left (D.DBError D.DBSystemError ("user error (open: Invalid argument: " +| dbPath |+ ": exists (error_if_exists is true))"))

    describe "Database usage tests" $ do
        it "Write and Read KBlock Meta" $ withDbAbsence dbPath $ do
            eRes <- evalNode $ putGetKBlockMetaNode dbPath
            eRes `shouldBe` Right True

        it "Write and Read KBlock Meta in separate runs" $ withDbAbsence dbPath $ do
            eInitialized <- evalNode $ dbInitNode cfg1
            eInitialized `shouldBe` Right ()

            eStoreResult <- evalNode $ putKBlockMetaNode cfg1
            eStoreResult `shouldBe` Right ()

            eValue <- evalNode $ getKBlockMetaNode cfg1
            eValue `shouldBe` Right kBlock1MetaValue

        it "Read unexisting KBlock Meta" $ withDbPresence dbPath $ do
            eInitialized <- evalNode $ dbInitNode cfg1
            eInitialized `shouldBe` Right ()

            eValue <- evalNode $ getKBlockMetaNode cfg1
            eValue `shouldBe` Left (D.DBError D.KeyNotFound (show $ D.fromStringHash $ D.toHash kBlock1))

