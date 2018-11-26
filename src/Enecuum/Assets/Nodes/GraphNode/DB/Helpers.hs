module Enecuum.Assets.Nodes.GraphNode.DB.Helpers where

import           Enecuum.Prelude

import qualified Enecuum.Blockchain.DB                           as D
import qualified Enecuum.Blockchain.DB.Lens                      as Lens
import qualified Enecuum.Blockchain.Lens                         as Lens
import qualified Enecuum.Domain                                  as D
import qualified Enecuum.Language                                as L

import           Enecuum.Assets.Nodes.GraphNode.GraphServiceData (GraphServiceData (..))

withKBlocksDB
    :: forall s db a
    .  Lens.HasKBlocksDB s (D.Storage db)
    => s
    -> L.DatabaseL db a
    -> L.NodeL a
withKBlocksDB dbModel = L.withDatabase (dbModel ^. Lens.kBlocksDB)

withKBlocksMetaDB
    :: forall s db a
    .  Lens.HasKBlocksMetaDB s (D.Storage db)
    => s
    -> L.DatabaseL db a
    -> L.NodeL a
withKBlocksMetaDB dbModel = L.withDatabase (dbModel ^. Lens.kBlocksMetaDB)

withMBlocksDB
    :: forall s db a
    .  Lens.HasMBlocksDB s (D.Storage db)
    => s
    -> L.DatabaseL db a
    -> L.NodeL a
withMBlocksDB dbModel = L.withDatabase (dbModel ^. Lens.mBlocksDB)

withMBlocksMetaDB
    :: forall s db a
    .  Lens.HasMBlocksMetaDB s (D.Storage db)
    => s
    -> L.DatabaseL db a
    -> L.NodeL a
withMBlocksMetaDB dbModel = L.withDatabase (dbModel ^. Lens.mBlocksMetaDB)

withTransactionsDB
    :: forall s db a
    .  Lens.HasTransactionsDB s (D.Storage db)
    => s
    -> L.DatabaseL db a
    -> L.NodeL a
withTransactionsDB dbModel = L.withDatabase (dbModel ^. Lens.transactionsDB)

withTransactionsMetaDB
    :: forall s db a
    .  Lens.HasTransactionsMetaDB s (D.Storage db)
    => s
    -> L.DatabaseL db a
    -> L.NodeL a
withTransactionsMetaDB dbModel = L.withDatabase (dbModel ^. Lens.transactionsMetaDB)

withDBModel :: GraphServiceData -> (D.DBModel -> L.NodeL ()) -> L.NodeL ()
withDBModel (_db -> Just dbModel) act = act dbModel
withDBModel _ _                       = pure ()

-- | On `Right val`, evals `action` with `val`.
-- On `Left err`, returns `Left err`.
withResult
    :: Applicative f
    => Either a t
    -> (t -> f (Either a b))
    -> f (Either a b)
withResult (Left err)     _      = pure $ Left err
withResult (Right result) action = action result

-- | On `Right val`, evals `action` with `val` and returns a list of successes.
-- On `Left err`, logs error except it is KeyNotFound and returns empty list.
materialize
    :: Monad m
    => L.Logger m
    => D.DBResult t
    -> (t -> m [b])
    -> m [b]
materialize (Right result)                     action = action result
materialize (Left (D.DBError D.KeyNotFound _)) _      = pure []
materialize (Left err)                         _      = do
    L.logError $ show err
    pure []
