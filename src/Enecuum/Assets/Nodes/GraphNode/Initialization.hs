{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE TemplateHaskell        #-}

-- Initialization specific to graph service.
module Enecuum.Assets.Nodes.GraphNode.Initialization where

import           Control.Lens                                    (makeFieldsNoPrefix)
import           Data.HGraph.StringHashable
import qualified Data.Map                                        as Map
import           Enecuum.Prelude
import           System.FilePath                                 ((</>))

import qualified Enecuum.Assets.Nodes.CLens                      as CLens
import           Enecuum.Assets.Nodes.GraphNode.Config
import           Enecuum.Assets.Nodes.GraphNode.GraphServiceData
import qualified Enecuum.Assets.System.Directory                 as L
import qualified Enecuum.Blockchain.DB                           as D
import qualified Enecuum.Blockchain.Lens                         as Lens
import qualified Enecuum.Domain                                  as D
import qualified Enecuum.Language                                as L

initDb :: forall db. D.DB db => D.DBOptions -> FilePath -> L.NodeL (D.DBResult (D.Storage db))
initDb options dbModelPath = do
    let dbPath   = dbModelPath </> D.getDbName @db <> ".db"
    let dbConfig = D.DBConfig dbPath options
    eDb <- L.initDatabase dbConfig
    whenRight eDb $ const $ L.logInfo  $ "Database initialized: " +| dbPath |+ ""
    whenLeft  eDb $ \err -> L.logError $ "Database initialization failed." <>
        "\n    Path: " +| dbPath |+
        "\n    Error: " +|| err ||+ ""
    pure eDb

initDBModel' :: FilePath -> D.DBOptions -> L.NodeL (Maybe D.DBModel)
initDBModel' dbModelPath options = do
    void $ L.createFilePath dbModelPath

    eKBlocksDb          <- initDb options dbModelPath
    eKBlocksMetaDb      <- initDb options dbModelPath
    eMBlocksDb          <- initDb options dbModelPath
    eMBlocksMetaDb      <- initDb options dbModelPath
    eTransactionsDb     <- initDb options dbModelPath
    eTransactionsMetaDb <- initDb options dbModelPath

    let eModel = D.DBModel
            <$> eKBlocksDb
            <*> eKBlocksMetaDb
            <*> eMBlocksDb
            <*> eMBlocksMetaDb
            <*> eTransactionsDb
            <*> eTransactionsMetaDb

    when (isLeft  eModel) $ L.logError $ "Failed to initialize DB model: " +| dbModelPath |+ "."
    when (isRight eModel) $ L.logInfo  $ "DB model initialized: "          +| dbModelPath |+ "."
    pure $ rightToMaybe eModel

initDBModel :: DBConfig -> L.NodeL (Maybe D.DBModel)
initDBModel dbConfig = do
    parentDir <- if dbConfig ^. CLens.useEnqHomeDir
        then L.getEnecuumDir
        else pure ""

    let dbModelPath = parentDir </> (dbConfig ^. CLens.dbModelName)
    initDBModel' dbModelPath $ dbConfig ^. CLens.dbOptions

-- | Initialization of graph node
graphServiceInitialization :: GraphServiceConfig -> L.NodeDefinitionL (Either Text GraphServiceData)
graphServiceInitialization serviceConfig = L.scenario $ do
    let dbConfig    = serviceConfig ^. CLens.dbConfig
    let useDb       = dbConfig ^. CLens.useDatabase
    let stopOnDbErr = dbConfig ^. CLens.stopOnDatabaseError

    mbDBModel <- if useDb
        then initDBModel dbConfig
        else pure Nothing

    g <- L.newGraph
    L.evalGraphIO g $ L.newNode $ D.KBlockContent D.genesisKBlock

    bottomKBlockHash <- L.newVarIO D.genesisHash
    topKBlockHash    <- L.newVarIO D.genesisHash

    let windowedGraph = D.WindowedGraph
          { D._graph            = g
          , D._bottomKBlockHash = bottomKBlockHash
          , D._topKBlockHash    = topKBlockHash
          }

    kBlockPending      <- L.newVarIO Map.empty
    transactionPending <- L.newVarIO Map.empty
    ledger             <- L.newVarIO Map.empty

    serviceData <- L.atomically
        $ GraphServiceData
            ( D.BlockchainData
                  { D._windowedGraph      = windowedGraph
                  , D._kBlockPending      = kBlockPending
                  , D._transactionPending = transactionPending
                  , D._ledger             = ledger
              }
            )
        <$> pure mbDBModel
        <*> L.newVar False
        <*> L.newVar False
        <*> L.newVar False

    let dbUsageFailed = useDb && stopOnDbErr && isNothing mbDBModel

    unless dbUsageFailed
        $ L.logInfo $ "Genesis block (" +|| D.genesisHash ||+ "): " +|| D.genesisKBlock ||+ "."

    if dbUsageFailed
        then pure $ Left "Database error."
        else pure $ Right serviceData
