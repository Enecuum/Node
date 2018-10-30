{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
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

import           Test.Hspec
import           Enecuum.Testing.Integrational

-- TODO: type of a
parseDBValue :: (Typeable a, FromJSON a) => D.DBValueRaw -> D.DBResult a
parseDBValue raw = case A.decode $ LBS.fromStrict raw of
    Nothing  -> Left $ D.DBError D.InvalidType ""
    Just val -> Right val

dbOpts :: D.DBOptions
dbOpts = D.DBOptions
    { D._createIfMissing = True
    , D._errorIfExists   = True
    }

-- kBlocks_meta (kBlock_hash -> kBlock_meta)
-- -----------------------------------------------------------------
-- 4z9ADFAWehl6XGW2/N+2keOgNR921st3oPSVxv08hTY= (0, "")

-- kBlocks (kBlock_idx|0 -> prev_hash, kBlock_idx|1 -> kBlock_data)
-- ------------------------------------------------------------
-- 0000000|0 AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=
-- 0000000|1 {number:0, nonce: 0, solver: 1}

data KBlocksMetaDB
data KBlockMetaEntity

data KBlocksDB
data KBlockPrevHashEntity
data KBlockEntity

class DBEntity entity src where
    data DBKey   entity :: *
    data DBValue entity :: *
    toDBKey   :: src -> DBKey   entity
    toDBValue :: src -> DBValue entity

class GetRawDBEntity entity where
    getRawDBKey   :: DBKey   entity -> D.DBKeyRaw
    getRawDBValue :: DBValue entity -> D.DBValueRaw

getRawDBEntity :: GetRawDBEntity entity => DBKey entity -> DBValue entity -> (D.DBKeyRaw, D.DBValueRaw)
getRawDBEntity dbKey dbVal = (getRawDBKey dbKey, getRawDBValue dbVal)

instance DBEntity KBlockMetaEntity D.KBlock where
    data DBKey   KBlockMetaEntity = KBlockMetaKey D.DBKeyRaw
        deriving (Show, Eq, Ord)
    data DBValue KBlockMetaEntity = KBlockMetaValue Integer
        deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
    toDBKey          = KBlockMetaKey . D.fromStringHash . D.toHash
    toDBValue kBlock = KBlockMetaValue $ kBlock ^. Lens.number

instance GetRawDBEntity KBlockMetaEntity where
    getRawDBKey (KBlockMetaKey k) = k
    getRawDBValue v = LBS.toStrict $ A.encode v

instance DBEntity KBlockPrevHashEntity Integer where
    data DBKey   KBlockPrevHashEntity = KBlockPrevHashKey D.StringHash
        deriving (Show, Eq, Ord)
    data DBValue KBlockPrevHashEntity = KBlockPrevHashValue D.StringHash
        deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
    toDBKey   idx = KBlockPrevHashKey $ D.StringHash $ encodeUtf8 @String $ printf "%07d0" idx
    toDBValue idx = error "2222222222222"

instance DBEntity KBlockEntity Integer where
    data DBKey   KBlockEntity = KBlockKey D.StringHash
        deriving (Show, Eq, Ord)
    data DBValue KBlockEntity = KBlockValue Integer Integer Integer D.StringHash
        deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
    toDBKey   idx = KBlockKey $ D.StringHash $ encodeUtf8 @String $ printf "%07d1" idx
    toDBValue idx = error "333333333333333"

instance GetRawDBEntity KBlockPrevHashEntity where
    getRawDBKey   (KBlockPrevHashKey k)   = D.fromStringHash k
    getRawDBValue (KBlockPrevHashValue k) = error "4444444444"

instance GetRawDBEntity KBlockEntity where
    getRawDBKey   (KBlockKey k)         = D.fromStringHash k
    getRawDBValue (KBlockValue _ _ _ _) = error "55555555"

putValue :: GetRawDBEntity spec => DBKey spec -> DBValue spec -> L.DatabaseL db (D.DBResult ())
putValue dbKey dbVal = let
    (rawK, rawV) = getRawDBEntity dbKey dbVal
    in L.putValue rawK rawV

type DBE spec = (DBKey spec, DBValue spec)

getEntity
    :: (FromJSON (DBValue spec), GetRawDBEntity spec, Typeable (DBValue spec))
    => DBKey spec
    -> L.DatabaseL db (D.DBResult (DBE spec))
getEntity dbKey = do
    eRawVal <- L.getValue $ getRawDBKey dbKey
    pure $ eRawVal >>= parseDBValue >>= (\dbVal -> Right (dbKey, dbVal))

getValue
    :: (FromJSON (DBValue spec), GetRawDBEntity spec, Typeable (DBValue spec))
    => DBKey spec
    -> L.DatabaseL db (D.DBResult (DBValue spec))
getValue dbKey = do
    eEntity <- getEntity dbKey
    pure $ eEntity >>= Right . snd

findValue
    :: (FromJSON (DBValue spec), GetRawDBEntity spec, Typeable (DBValue spec))
    => DBKey spec
    -> L.DatabaseL db (Maybe (DBValue spec))
findValue key = do
    eVal <- getValue key
    pure $ either (const Nothing) Just eVal

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
            $ putValue kBlock1MetaKey kBlock1MetaValue

getKBlockMetaNode :: D.DBConfig KBlocksMetaDB -> L.NodeDefinitionL (Either D.DBError (DBValue KBlockMetaEntity))
getKBlockMetaNode cfg = do
    eDB <- L.scenario $ L.initDatabase cfg
    case eDB of
        Left err -> pure $ Left err
        Right db -> L.scenario $ L.withDatabase db $ getValue kBlock1MetaKey

putGetKBlockMetaNode :: FilePath -> L.NodeDefinitionL (Either D.DBError Bool)
putGetKBlockMetaNode dbPath = do
    let cfg :: D.DBConfig KBlocksMetaDB = D.DBConfig dbPath dbOpts
    eDB <- L.scenario $ L.initDatabase cfg
    case eDB of
        Left err -> pure $ Left err
        Right db -> L.scenario $ L.withDatabase db $ do
            eRes <- putValue kBlock1MetaKey kBlock1MetaValue
            case eRes of
                Left err -> pure $ Left err
                Right _  -> do
                    eVal <- getValue kBlock1MetaKey
                    pure $ eVal >>= (\val2 -> Right (kBlock1MetaValue == val2))

toKBlock :: DBValue KBlockPrevHashEntity -> DBValue KBlockEntity -> D.KBlock
toKBlock (KBlockPrevHashValue prevHash) (KBlockValue t n nc s) = D.KBlock
    { D._time     = t
    , D._prevHash = prevHash
    , D._number   = n
    , D._nonce    = nc
    , D._solver   = s
    }

getKBlock :: NodeData -> Maybe (DBValue KBlockMetaEntity) -> L.NodeL (Maybe D.KBlock)
getKBlock _         Nothing = pure Nothing
getKBlock nodeData (Just (KBlockMetaValue kBlockIdx)) = do
    let key1 = toDBKey @KBlockPrevHashEntity kBlockIdx
    let key2 = toDBKey @KBlockEntity         kBlockIdx
    mbPrevHashValue <- L.withDatabase (nodeData ^. kBlocksDB) $ findValue key1
    mbKBlockValue   <- L.withDatabase (nodeData ^. kBlocksDB) $ findValue key2
    pure $ toKBlock <$> mbPrevHashValue <*> mbKBlockValue

kBlock1 :: D.KBlock
kBlock1 = D.KBlock
    { _time      = 0
    , _prevHash  = D.genesisHash
    , _number    = 1
    , _nonce     = 0
    , _solver    = D.genesisHash
    }

kBlock1MetaKey :: DBKey KBlockMetaEntity
kBlock1MetaKey = toDBKey kBlock1

kBlock1MetaValue :: DBValue KBlockMetaEntity
kBlock1MetaValue = toDBValue kBlock1

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

    describe "DB Entities tests" $ do
        it "ToDBKey test"          $ kBlock1MetaKey `shouldBe` KBlockMetaKey (D.fromStringHash $ D.toHash kBlock1)
        it "ToDBValue test"        $ kBlock1MetaValue `shouldBe` KBlockMetaValue 1
        it "GetRawDBEntity test"   $ getRawDBKey kBlock1MetaKey `shouldBe` D.fromStringHash (D.toHash kBlock1)
        it "Parse RawDBValue test" $ do
            let dbValueRaw = getRawDBValue kBlock1MetaValue
            parseDBValue dbValueRaw `shouldBe` Right kBlock1MetaValue

    describe "Database creation tests" $ do
        let cfg1 = D.DBConfig dbPath $ D.defaultDbOptions
                    { D._createIfMissing = True
                    , D._errorIfExists   = False
                    }
        let cfg2 = D.DBConfig dbPath $ D.defaultDbOptions
                    { D._createIfMissing = True
                    , D._errorIfExists   = True
                    }
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
            let cfg = D.DBConfig dbPath $ D.DBOptions
                    { D._createIfMissing = True
                    , D._errorIfExists   = False
                    }
            eInitialized <- evalNode $ dbInitNode cfg
            eInitialized `shouldBe` Right ()

            eStoreResult <- evalNode $ putKBlockMetaNode cfg
            eStoreResult `shouldBe` Right ()

            eValue <- evalNode $ getKBlockMetaNode cfg
            eValue `shouldBe` Right kBlock1MetaValue




