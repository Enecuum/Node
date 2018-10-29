{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PackageImports        #-}

module Enecuum.Tests.Integration.DatabaseSpec where

import           Enecuum.Prelude

import qualified Data.Aeson   as A
import qualified Data.Map     as M
import qualified Data.List    as List
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

-- findValue :: forall a m. (Typeable a, FromJSON a, Database m) => D.DBKey -> m (Either D.DBError a)
-- findValue key = do
--     mbRaw <- getValue key
--     case mbRaw of
--         Nothing  -> pure $ Left $ D.KeyNotFound key
--         Just raw -> case A.decode raw of
--             Nothing  -> pure $ Left $ D.InvalidType $ show $ typeOf (Proxy :: Proxy a)
--             Just val -> pure $ Right val

dbOpts =  D.DBOptions
    { D._createIfMissing = True
    , D._errorIfExists   = True
    }

findValue :: DBKey spec -> L.DatabaseL db (Maybe a)
findValue key = error "findValue not implemented."

toDBValue :: a -> D.DBValue db spec
toDBValue = error "toDBValue not implemented."

-- kBlocks_meta (kBlock_hash -> kBlock_meta)
-- -----------------------------------------------------------------
-- AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA= (AAAAAAA, "")

-- kBlocks (kBlock_idx|0 -> prev_hash, kBlock_idx|1 -> kBlock_data)
-- ------------------------------------------------------------
-- 0000000|0 AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=
-- 0000000|1 {number:0, nonce: 0, solver: 1}

data KBlocksMetaDB
data KBlockMetaValue = KBlockMetaValue Integer
data KBlockMetaKey

data KBlocksDB
data KBlockPrevHashValue = KBlockPrevHashValue D.StringHash
data KBlockValue = KBlockValue Integer Integer Integer D.StringHash
data KBlockPrevHashKey
data KBlockKey

class ToDBKey entity src where
    data DBKey entity :: *
    toDBKey :: src -> DBKey entity

class GetDBKey entity where
    getRawDBKey :: DBKey entity -> D.DBKeyRaw

instance ToDBKey KBlockMetaKey D.KBlock where
    data DBKey KBlockMetaKey = KBlockMetaKey D.DBKeyRaw
    toDBKey = KBlockMetaKey . LBS.fromStrict . D.fromStringHash . D.toHash

instance GetDBKey KBlockMetaKey where
    getRawDBKey (KBlockMetaKey k) = k

instance ToDBKey KBlockPrevHashKey KBlockMetaValue where
    data DBKey KBlockPrevHashKey = KBlockPrevHashKey D.StringHash
    toDBKey (KBlockMetaValue blockIdx) = KBlockPrevHashKey $ D.StringHash $ encodeUtf8 @String k
        where
            k = printf "%07d0" blockIdx

instance ToDBKey KBlockKey KBlockMetaValue where
    data DBKey KBlockKey = KBlockKey D.StringHash
    toDBKey (KBlockMetaValue blockIdx) = KBlockKey $ D.StringHash $ encodeUtf8 @String k
        where
            k = printf "%07d1" blockIdx

instance GetDBKey KBlockPrevHashKey where
    getRawDBKey (KBlockPrevHashKey k) = LBS.fromStrict $ D.fromStringHash k

instance GetDBKey KBlockKey where
    getRawDBKey (KBlockKey k) = LBS.fromStrict $ D.fromStringHash k


putValue :: GetDBKey spec => DBKey spec -> D.DBValue db spec -> L.DatabaseL db ()
putValue dbKey (D.DBValue val) = L.putValue (getRawDBKey dbKey) val

data NodeData = NodeData
    { _kBlocksDB     :: D.Storage KBlocksDB
    , _kBlocksMetaDB :: D.Storage KBlocksMetaDB
    }

makeFieldsNoPrefix ''NodeData

getKBlockMeta :: NodeData -> D.KBlock -> L.NodeL (Maybe KBlockMetaValue)
getKBlockMeta nodeData kBlock = do
    let key = toDBKey @KBlockMetaKey kBlock
    L.withDatabase (nodeData ^. kBlocksMetaDB) $ findValue key

writeKBlockMeta :: L.DatabaseL KBlocksMetaDB ()
writeKBlockMeta = do
    let key = toDBKey @KBlockMetaKey kBlock1
    let val = toDBValue kBlock1
    putValue key val

readKBlockMeta :: L.DatabaseL KBlocksMetaDB (Maybe KBlockMetaValue)
readKBlockMeta = pure Nothing

dbKBlockMetaNode :: FilePath -> L.NodeDefinitionL (Either Text ())
dbKBlockMetaNode dbPath = do
    let cfg :: D.DBConfig KBlocksMetaDB = D.DBConfig dbPath dbOpts
    eDB <- L.scenario $ L.initDatabase cfg
    case eDB of
        Left err -> pure $ Left err
        Right db -> L.scenario $ L.withDatabase db $ do
            writeKBlockMeta
            readKBlockMeta
            pure (Right ())

toKBlock :: KBlockPrevHashValue -> KBlockValue -> D.KBlock
toKBlock (KBlockPrevHashValue prevHash) (KBlockValue t n nc s) = D.KBlock
    { D._time     = t
    , D._prevHash = prevHash
    , D._number   = n
    , D._nonce    = nc
    , D._solver   = s
    }

getKBlock :: NodeData -> Maybe KBlockMetaValue -> L.NodeL (Maybe D.KBlock)
getKBlock _         Nothing = pure Nothing
getKBlock nodeData (Just e) = do
    let key1 = toDBKey @KBlockPrevHashKey e
    let key2 = toDBKey @KBlockKey e
    mbPrevHashValue <- L.withDatabase (nodeData ^. kBlocksDB) $ findValue key1
    mbKBlockValue   <- L.withDatabase (nodeData ^. kBlocksDB) $ findValue key2
    pure $ toKBlock <$> mbPrevHashValue <*> mbKBlockValue

buildGraph :: NodeData -> D.StringHash -> L.NodeL (Maybe )
buildGraph nodeData kBlockHash = do
    mbMeta   <- getKBlockMeta nodeData kBlock

    pure Nothing

kBlock1 = D.KBlock
    { _time      = 0
    , _prevHash  = D.genesisHash
    , _number    = 1
    , _nonce     = 0
    , _solver    = D.genesisHash
    }

kBlock2 = D.KBlock
    { _time      = 0
    , _prevHash  = D.toHash kBlock1
    , _number    = 2
    , _nonce     = 0
    , _solver    = D.genesisHash
    }

kBlock3 = D.KBlock
    { _time      = 0
    , _prevHash  = D.toHash kBlock2
    , _number    = 3
    , _nonce     = 0
    , _solver    = D.genesisHash
    }

dbInitNode :: D.DBConfig db -> L.NodeDefinitionL (Either Text ())
dbInitNode cfg = do
    eDB <- L.scenario $ L.initDatabase cfg
    case eDB of
        Left err -> pure $ Left err
        Right _  -> pure $ Right ()

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
    describe "Database creation tests" $ do
        it "DB is missing, create, errorIfExists False, no errors expected" $ withDbAbsence dbPath $ do
            let cfg = D.DBConfig dbPath $ D.DBOptions
                    { D._createIfMissing = True
                    , D._errorIfExists   = False
                    }
            eRes <- evalNode $ dbInitNode cfg
            eRes `shouldBe` Right ()

        it "DB is missing, create, errorIfExists True, no errors expected" $ withDbAbsence dbPath $ do
            let cfg = D.DBConfig dbPath $ D.DBOptions
                    { D._createIfMissing = True
                    , D._errorIfExists   = True
                    }
            eRes <- evalNode $ dbInitNode cfg
            eRes `shouldBe` Right ()

        it "DB is present, create, errorIfExists False, no errors expected" $ withDbPresence dbPath $ do
            let cfg = D.DBConfig dbPath $ D.DBOptions
                    { D._createIfMissing = True
                    , D._errorIfExists   = False
                    }
            eRes <- evalNode $ dbInitNode cfg
            eRes `shouldBe` Right ()

        it "DB is present, create, errorIfExists False, errors expected" $ withDbPresence dbPath $ do
            let cfg = D.DBConfig dbPath $ D.DBOptions
                    { D._createIfMissing = True
                    , D._errorIfExists   = True
                    }
            eRes <- evalNode $ dbInitNode cfg
            eRes `shouldBe` (Left $ "user error (open: Invalid argument: " +| dbPath |+ ": exists (error_if_exists is true))")

    describe "Database usage tests" $ do
        it "Test1" $ do
            eRes <- evalNode $ dbKBlockMetaNode dbPath
            eRes `shouldBe` Right ()
