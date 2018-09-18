module Enecuum.Framework.Node.Interpreter where

import Enecuum.Prelude
import Control.Monad.Free

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L
import qualified Enecuum.Framework.Lens             as Lens

import Enecuum.Framework.Networking.Interpreter
import Enecuum.Framework.NetworkModel.Interpreter
import Enecuum.Framework.Node.Language
import Enecuum.Core.Interpreter


-- | Interpret NodeL. Does nothing ATM.
--interpretNodeL (L.Dummy) = L.logInfo "L.Dummy"
--
interpretNodeL :: NodeF a -> IO a
interpretNodeL (L.EvalGraph graphModel next) = undefined
-- | Eval networking.
interpretNodeL (L.EvalNetworking networking next) = 
    next <$> runNetworkingL networking
-- | Eval core effect.
interpretNodeL (L.EvalCoreEffectNodeF coreEffects next) =
    next <$> runCoreEffectF coreEffects


-- | Runs node model. Runs interpreters for the underlying languages.
runNodeModel :: Free NodeF a -> IO a
runNodeModel = foldFree interpretNodeL
