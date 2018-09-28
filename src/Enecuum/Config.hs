{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Enecuum.Config where

import           Enecuum.Prelude

import qualified Data.ByteString.Lazy          as L
import qualified Data.Aeson                    as A

import           Enecuum.Core.Types.Logger     (LoggerConfig)

data NodeRole = BootNode | MasterNode | NetworkNode
  deriving (Generic, FromJSON, Show, Read, Eq, Ord )

data Scenario = LedgerBalance | Sync
  deriving (Generic, FromJSON, Show, Read, Eq, Ord )

data ScenarioRole = Respondent | Interviewer
  deriving (Generic, FromJSON, Show, Read, Eq, Ord)

data ScenarioNode = ScenarioNode
  { nodeRole     :: NodeRole
  , scenario     :: Scenario
  , scenarioRole :: ScenarioRole
  } deriving (Generic, FromJSON, Show, Read, Eq, Ord)

data Config = Config
  { bootNodeAddress :: Text
  , scenarioNode :: [ScenarioNode]
  , extPort :: Int
  , loggerConfig :: LoggerConfig
  }
  deriving (Generic, FromJSON)

withConfig :: String -> (Config -> IO ()) -> IO ()
withConfig configName act = do
  configContents <- L.readFile configName
  case A.decode configContents of
    Nothing     -> error "Please, specify config file correctly"
    Just config -> act config
