module Enecuum.Blockchain.DB.Model where

import           Enecuum.Prelude
import           System.FilePath ((</>))

import           Enecuum.Blockchain.DB.KBlock     (KBlocksDB)
import           Enecuum.Blockchain.DB.KBlockMeta (KBlocksMetaDB)
import qualified Enecuum.Language                 as L
import qualified Enecuum.Domain                   as D
import qualified Enecuum.Assets.System.Directory  as L


data DBModel = DBModel
    { _kBlocksDB     :: D.Storage KBlocksDB
    , _kBlocksMetaDB :: D.Storage KBlocksMetaDB
    }

dbExt :: FilePath
dbExt = ".db"

initDb :: forall db. D.DB db => D.DBOptions -> FilePath -> L.NodeL (D.DBResult (D.Storage db))
initDb options dbModelPath = do
    let dbPath   = dbModelPath </> D.getDbName @db </> dbExt
    let dbConfig = D.DBConfig dbPath options
    eDb <- L.initDatabase dbConfig
    whenRight eDb $ const $ L.logInfo  $ "Database initialized: " +| dbPath |+ ""
    whenLeft  eDb $ \err -> L.logError $ "Database initialization failed." <>
        "\n    Path: " +| dbPath |+ 
        "\n    Error: " +|| err ||+ ""
    pure eDb

initDBModel :: D.DBOptions -> FilePath -> L.NodeL (Either Text DBModel)
initDBModel options dbModelName = do
    enqDir <- L.getEnecuumDir
    let dbModelPath = enqDir </> dbModelName
    void $ L.createFilePath dbModelPath

    eKBlocksDb     <- initDb options dbModelPath
    eKBlocksMetaDb <- initDb options dbModelPath

    let eModel = DBModel
            <$> eKBlocksDb
            <*> eKBlocksMetaDb

    case eModel of
        Left _        -> pure $ Left "Failed to initialize DB model."
        Right dbModel -> pure $ Right dbModel
