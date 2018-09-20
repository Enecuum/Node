module App.Initialize where

import           Enecuum.Config  (Config(..))
import           Enecuum.Prelude
import Enecuum.Assets.Scenarios

initialize :: Config -> IO ()
initialize config = do
    when (bootNode config) $ runBootNode config
    when (masterNode config) $ runMasterNode config
    pure ()
