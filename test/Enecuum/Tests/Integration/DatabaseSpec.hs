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

import qualified Data.Aeson   as A
import qualified Data.Map     as M
import qualified Data.List    as List
import           Control.Lens (makeFieldsNoPrefix)
import qualified "rocksdb-haskell" Database.RocksDB as Rocks

import           Enecuum.Prelude
import qualified System.Directory as Dir
import           System.FilePath as FP ((</>))

import           Enecuum.Interpreters                         (runNodeDefinitionL)
import qualified Enecuum.Runtime                              as R
import qualified Enecuum.Framework.NodeDefinition.Interpreter as R
import qualified Enecuum.Domain                               as D
import qualified Enecuum.Language                             as L

import           Test.Hspec
import           Enecuum.Testing.Integrational


data KBlocksMetaDB
data KBlockMetaEntity = KBlockMetaEntity D.DBIndex
type KBlockMetaEntityKey = D.DBKey KBlocksMetaDB KBlockMetaEntity

data KBlocksDB
data KBlockPrevHashEntity = KBlockPrevHashEntity D.StringHash
type KBlockPrevHashEntityKey = D.DBKey KBlocksDB KBlockPrevHashEntity

data KBlockEntity = KBlockEntity
    { _time      :: Integer
    , _number    :: Integer
    , _nonce     :: Integer
    , _solver    :: D.StringHash
    }
type KBlockEntityKey = D.DBKey KBlocksDB KBlockEntity


-- findValue :: forall a m. (Typeable a, FromJSON a, Database m) => D.DBKey -> m (Either D.DBError a)
-- findValue key = do
--     mbRaw <- getValue key
--     case mbRaw of
--         Nothing  -> pure $ Left $ D.KeyNotFound key
--         Just raw -> case A.decode raw of
--             Nothing  -> pure $ Left $ D.InvalidType $ show $ typeOf (Proxy :: Proxy a)
--             Just val -> pure $ Right val


findValue :: D.DBKey db spec -> L.DatabaseL db (Maybe a)
findValue key = undefined

toDBKey :: a -> D.DBKey db spec
toDBKey = undefined


data NodeData = NodeData
    { _kBlocksDB     :: D.Storage KBlocksDB
    , _kBlocksMetaDB :: D.Storage KBlocksMetaDB
    }

makeFieldsNoPrefix ''NodeData

getKBlockMeta :: NodeData -> D.KBlock -> L.NodeL (Maybe KBlockMetaEntity)
getKBlockMeta nodeData kBlock = do
    let key :: KBlockMetaEntityKey = toDBKey kBlock
    L.withDatabase (nodeData ^. kBlocksMetaDB) $ findValue key

toKBlock :: KBlockPrevHashEntity -> KBlockEntity -> D.KBlock
toKBlock (KBlockPrevHashEntity prevHash) (KBlockEntity t n nc s) = D.KBlock
    { D._time     = t
    , D._prevHash = prevHash
    , D._number   = n
    , D._nonce    = nc
    , D._solver   = s
    }

getKBlock :: NodeData -> Maybe KBlockMetaEntity -> L.NodeL (Maybe D.KBlock)
getKBlock _         Nothing = pure Nothing
getKBlock nodeData (Just e) = do
    let key1 :: KBlockPrevHashEntityKey = toDBKey e
    let key2 :: KBlockEntityKey         = toDBKey e
    mbPrevHashEntity <- L.withDatabase (nodeData ^. kBlocksDB) $ findValue key1
    mbKBlockEntity   <- L.withDatabase (nodeData ^. kBlocksDB) $ findValue key2
    pure $ toKBlock <$> mbPrevHashEntity <*> mbKBlockEntity


getNextKBlock :: NodeData -> D.KBlock -> L.NodeL (Maybe D.KBlock)
getNextKBlock nodeData kBlock = do
    mbMeta   <- getKBlockMeta nodeData kBlock
    mbKBlock <- getKBlock nodeData mbMeta
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

dbNode :: D.DBConfig db -> L.NodeDefinitionL (Either Text ())
dbNode cfg = do
    eDB <- L.scenario $ L.initDatabase cfg
    L.delay $ 1000 * 500
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
    db <- Rocks.open dbPath $ Rocks.defaultOptions {Rocks.createIfMissing = True, Rocks.errorIfExists = False}
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
spec = describe "Database support tests" $ do
    dbPath <- runIO $ mkDbPath "test.db"
    it "DB is missing, create, errorIfExists False, no errors expected" $ withDbAbsence dbPath $ do
        let cfg = D.DBConfig dbPath $ D.DBOptions
                { D._createIfMissing = True
                , D._errorIfExists   = False
                }
        eRes <- evalNode $ dbNode cfg
        eRes `shouldBe` Right ()

    it "DB is missing, create, errorIfExists True, no errors expected" $ withDbAbsence dbPath $ do
        let cfg = D.DBConfig dbPath $ D.DBOptions
                { D._createIfMissing = True
                , D._errorIfExists   = True
                }
        eRes <- evalNode $ dbNode cfg
        eRes `shouldBe` Right ()

    it "DB is present, create, errorIfExists False, no errors expected" $ withDbPresence dbPath $ do
        let cfg = D.DBConfig dbPath $ D.DBOptions
                { D._createIfMissing = True
                , D._errorIfExists   = False
                }
        eRes <- evalNode $ dbNode cfg
        eRes `shouldBe` Right ()

    it "DB is present, create, errorIfExists False, errors expected" $ withDbPresence dbPath $ do
        let cfg = D.DBConfig dbPath $ D.DBOptions
                { D._createIfMissing = True
                , D._errorIfExists   = True
                }
        eRes <- evalNode $ dbNode cfg
        eRes `shouldBe` (Left $ "user error (open: Invalid argument: " +| dbPath |+ ": exists (error_if_exists is true))")
