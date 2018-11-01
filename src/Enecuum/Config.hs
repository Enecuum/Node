{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Enecuum.Config where

import           Enecuum.Prelude

import qualified Data.ByteString.Lazy          as L
import qualified Data.Aeson                    as A

import           Enecuum.Core.Types.Logger     (LoggerConfig(..))
import           Enecuum.Language              (NodeDefinitionL)

data Config node = Config
    { bootNodeAddress :: Text
    , node            :: node
    , nodeScenario    :: NodeScenario node
    , nodeConfig      :: NodeConfig node
    , extPort         :: Int
    , loggerConfig    :: LoggerConfig
    }
    deriving (Generic)

instance (FromJSON node, FromJSON (NodeScenario node), FromJSON (NodeConfig node)) => FromJSON (Config node)
instance (ToJSON   node, ToJSON   (NodeScenario node), ToJSON   (NodeConfig node)) => ToJSON   (Config node)

data family NodeConfig node :: *

class Node node where
    data NodeScenario node :: *
    getNodeScript :: NodeScenario node -> NodeConfig node -> NodeDefinitionL ()

nodeConfigJsonOptions :: A.Options
nodeConfigJsonOptions = A.defaultOptions
    { A.unwrapUnaryRecords    = False
    , A.tagSingleConstructors = True
    }

withConfig :: FilePath -> (LByteString -> IO ()) -> IO ()
withConfig configName act = act =<< L.readFile configName

tryParseConfig
    :: (FromJSON node, FromJSON (NodeScenario node), FromJSON (NodeConfig node))
    => LByteString
    -> Maybe (Config node)
tryParseConfig = A.decode
