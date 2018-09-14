module Enecuum.Framework.NodeDefinition.Interpreter where

--
import Enecuum.Prelude

import           Eff                                ( handleRelay )

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L

import Enecuum.Framework.Node.Interpreter



-- | Interpret NodeDefinitionL.
interpretNodeDefinitionL
    :: L.NodeDefinitionL a
    -> Eff '[L.LoggerL, SIO, Exc SomeException] a
interpretNodeDefinitionL (L.NodeTag tag) = do
    L.logInfo $ "Node tag: " +| tag |+ ""
interpretNodeDefinitionL (L.Initialization initScript) = do
    L.logInfo "Initialization"
    runNodeModel initScript
interpretNodeDefinitionL (L.Serving handlersF) = do
    L.logInfo "Function serving is undefined"

interpretNodeDefinitionL (L.ServingRpc ls) = do
    L.logInfo "Start of servingRpc"


-- | Runs node definition language with node runtime.
runNodeDefinitionL
  :: Eff '[L.NodeDefinitionL, L.LoggerL, SIO, Exc SomeException] a
  -> Eff '[L.LoggerL, SIO, Exc SomeException] a
runNodeDefinitionL = handleRelay pure ( (>>=) . interpretNodeDefinitionL)