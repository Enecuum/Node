module App.Initialize where

import           Enecuum.Prelude

import           Enecuum.Config  (Config(..))
import qualified Enecuum.Assets.Scenarios as S
import           Enecuum.Interpreters (runNodeDefinitionL)
import           Enecuum.Runtime (createNodeRuntime)

initialize :: Config -> IO ()
initialize config = do
    nodeRt <- createNodeRuntime
    when (bootNode config)   $ runNodeDefinitionL nodeRt $ S.bootNode   config
    when (masterNode config) $ runNodeDefinitionL nodeRt $ S.masterNode config
    pure ()
