{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances  #-}

module Enecuum.Config where

import qualified Data.Aeson                as A
import           Data.Aeson.Extra          (noLensPrefix)
import qualified Data.ByteString.Internal  as BSI
import qualified Data.ByteString.Lazy      as L
import           Data.Yaml                 as A
import           Enecuum.Core.Types.Logger (LoggerConfig (..))
import           Enecuum.Language          (NodeDefinitionL)
import           Enecuum.Prelude

-- | General config for node.
-- Separate config types for a node can be specified.
-- N.B., ToJSON and FromJSON instances should be declared using 'nodeConfigJsonOptions'.
data Config node = Config
    { node         :: node                 -- ^ Node tag.
    , nodeScenario :: NodeScenario node    -- ^ Node scenario. It's possible to have several scenarios for node.
    , nodeConfig   :: NodeConfig node      -- ^ Node config. Different scenarios have the same config.
    , loggerConfig :: LoggerConfig         -- ^ Logger config.
    }
    deriving (Generic)

instance (FromJSON node, FromJSON (NodeScenario node), FromJSON (NodeConfig node)) => FromJSON (Config node)
instance (ToJSON   node, ToJSON   (NodeScenario node), ToJSON   (NodeConfig node)) => ToJSON   (Config node)

-- | Represents a config type for a particular node.
data family NodeConfig node :: *

-- | Represents a definition of node scenarios available.
class Node node where
    data NodeScenario node :: *
    getNodeScript :: NodeScenario node -> NodeConfig node -> NodeDefinitionL ()

-- | Options for ToJSON / FromJSON instances for configs.
-- These options take care about correct parsing of enum and data types.
nodeConfigJsonOptions :: A.Options
nodeConfigJsonOptions = noLensPrefix
    { A.unwrapUnaryRecords    = False
    , A.tagSingleConstructors = True
    }

-- | Reads a config file and evals some action with the contents.
withConfig :: FilePath -> (LByteString -> IO ()) -> IO ()
withConfig configName act = act =<< L.readFile configName

-- | Tries to parse config according to the type @node@ passed.
tryParseConfig
    :: (FromJSON node, FromJSON (NodeScenario node), FromJSON (NodeConfig node))
    => BSI.ByteString
    -> Either ParseException (Config node)
tryParseConfig = A.decodeEither'
