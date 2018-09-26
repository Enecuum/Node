-- TODO: this is almost exact copy-paste from testing runtime. Unify it.

module Enecuum.Framework.State.Interpreter where

import Enecuum.Prelude

import qualified Data.Map                                               as Map
import           Unsafe.Coerce                                          (unsafeCoerce)

import qualified Enecuum.Core.Types                                     as D
import qualified Enecuum.Framework.Language                             as L
import qualified Enecuum.Framework.Domain                               as D
import qualified Enecuum.Framework.Runtime                              as Rt
import qualified Enecuum.Framework.Lens                                 as Lens
import qualified Enecuum.Framework.RLens                                as RLens

import qualified Data.ByteString.Base64  as Base64
import qualified Crypto.Hash.SHA256      as SHA
import           Data.HGraph.StringHashable (StringHash (..), StringHashable, toHash)

newtype VarNumber = VarNumber Int

instance StringHashable VarNumber where
  toHash (VarNumber n) = StringHash . Base64.encode . SHA.hash $ show ("VarNumber " +|| n ||+ "" :: String)

getVarNumber :: Rt.NodeRuntime -> STM VarNumber
getVarNumber nodeRt = do
  number <- takeTMVar $ nodeRt ^. RLens.varCounter
  putTMVar (nodeRt ^. RLens.varCounter) $ number + 1
  pure $ VarNumber number

newVar' :: Rt.NodeRuntime -> a -> STM D.VarId
newVar' nodeRt a = do
  varNumber <- getVarNumber nodeRt
  tvar      <- newTVar $ unsafeCoerce a
  nodeState <- takeTMVar $ nodeRt ^. RLens.state
  let varId = D.toHash varNumber
  putTMVar (nodeRt ^. RLens.state) $ Map.insert varId (Rt.VarHandle varId tvar) nodeState
  pure varId

readVar' :: Rt.NodeRuntime -> D.StateVar a -> STM a
readVar' nodeRt (view Lens.varId -> varId) = do
  nodeState <- readTMVar $ nodeRt ^. RLens.state
  case Map.lookup varId nodeState of
    Nothing                    -> error $ "Var not found: " +|| varId ||+ "."
    Just (Rt.VarHandle _ tvar) -> unsafeCoerce <$> readTVar tvar

writeVar' :: Rt.NodeRuntime -> D.StateVar a -> a -> STM ()
writeVar' nodeRt (view Lens.varId -> varId) val = do
  nodeState <- readTMVar $ nodeRt ^. RLens.state
  case Map.lookup varId nodeState of
    Nothing                    -> error $ "Var not found: " +|| varId ||+ "."
    Just (Rt.VarHandle _ tvar) -> writeTVar tvar $ unsafeCoerce val


-- | Interpret StateL as STM.
interpretStateL :: Rt.NodeRuntime -> L.StateF a -> STM a

interpretStateL nodeRt (L.NewVar val next) =
  next . D.StateVar <$> newVar' nodeRt val

interpretStateL nodeRt (L.ReadVar var next) =
  next <$> readVar' nodeRt var

interpretStateL nodeRt (L.WriteVar var val next) =
  next <$> writeVar' nodeRt var val

interpretStateL _ (L.EvalGraph (L.GraphAction stmRunner _ act) next) =
  next <$> stmRunner act

-- | Runs state model as STM.
runStateL :: Rt.NodeRuntime -> L.StateL a -> STM a
runStateL nodeRt = foldFree (interpretStateL nodeRt)
