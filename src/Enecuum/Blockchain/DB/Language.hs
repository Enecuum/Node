module Enecuum.Blockchain.DB.Language where

import           Enecuum.Prelude
import           System.FilePath ((</>))

import qualified Enecuum.Core.Language            as L
import qualified Enecuum.Core.Types               as D
import qualified Enecuum.Framework.Language       as L
import qualified Enecuum.Framework.Domain         as D

import           Enecuum.Blockchain.DB.Entities
import           Enecuum.Blockchain.DB.Model      (KBlocksDB, KBlocksMetaDB, DBModel (..), dbExt)

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

initDBModel :: FilePath -> D.DBOptions -> L.NodeL (Maybe DBModel)
initDBModel dbModelPath options = do
    void $ L.createFilePath dbModelPath

    eKBlocksDb     <- initDb options dbModelPath
    eKBlocksMetaDb <- initDb options dbModelPath

    let eModel = DBModel
            <$> eKBlocksDb
            <*> eKBlocksMetaDb

    when (isLeft eModel)  $ L.logError $ "Failed to initialize DB model: " +| dbModelPath |+ "."
    when (isRight eModel) $ L.logInfo  $ "DB model initialized: " +| dbModelPath |+ "."
    pure $ rightToMaybe eModel
