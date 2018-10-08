{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Enecuum.Config where

import           Enecuum.Prelude

import qualified Data.ByteString.Lazy          as L
import qualified Data.Aeson                    as A

import           Enecuum.Core.Types.Logger     (LoggerConfig(..))
import           System.FilePath.Windows       (dropFileName)
import           System.Directory (createDirectoryIfMissing)

data NodeRole = BootNode | MasterNode | NetworkNode | PoW | PoA | Client | GraphNode
  deriving (Generic, FromJSON, Show, Read, Eq, Ord )

data Scenario = LedgerBalance | SyncChain | SyncKblock | Full
  deriving (Generic, FromJSON, Show, Read, Eq, Ord)

data ScenarioRole = Respondent | Interviewer | Soly
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

withConfig :: FilePath -> (Config -> IO ()) -> IO ()
withConfig configName act = act =<< getConfigBase configName

getConfigBase :: FilePath -> IO Config
getConfigBase configName = do
  configContents <- L.readFile configName
  case A.decode configContents of
    Nothing     -> error "Please, specify config file correctly"
    Just config -> pure config

logConfig :: FilePath -> IO LoggerConfig
logConfig configName = do
  config <- getConfigBase configName
  let logConf@(LoggerConfig _ _ logFile _) = loggerConfig config
      dir = dropFileName logFile
  createDirectoryIfMissing True dir
  pure logConf
