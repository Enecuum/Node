module Enecuum.Testing.Framework.Interpreters.State where

import Enecuum.Prelude

import qualified Crypto.Hash.SHA256         as SHA
import qualified Data.ByteString.Base64     as Base64
import qualified Data.Map                   as Map
import           Data.HGraph.StringHashable (StringHash (..), StringHashable, toHash)
import           Unsafe.Coerce              (unsafeCoerce)

import qualified Enecuum.Language as L
import qualified Enecuum.Domain as D
import qualified Enecuum.Framework.Lens as Lens

import qualified Enecuum.Testing.RLens as RLens
import qualified Enecuum.Testing.Types as T
import           Enecuum.Core.HGraph.Interpreters.STM (runHGraphSTM)

newtype VarNumber = VarNumber Int

instance StringHashable VarNumber where
  toHash (VarNumber n) = StringHash . Base64.encode . SHA.hash $ show ("VarNumber " +|| n ||+ "" :: String)

-- TODO: this is almost copy-pasted to real runtime.

getVarNumber :: T.NodeRuntime -> STM VarNumber
getVarNumber nodeRt = do
    number <- T.getNextId nodeRt
    pure $ VarNumber number

newVar' :: T.NodeRuntime -> a -> STM D.VarId
newVar' nodeRt a = do
    varNumber <- getVarNumber nodeRt
    tvar      <- newTVar $ unsafeCoerce a
    nodeState <- takeTMVar $ nodeRt ^. RLens.state
    let varId = D.toHash varNumber
    putTMVar (nodeRt ^. RLens.state) $ Map.insert varId (T.VarHandle varId tvar) nodeState
    pure varId

readVar' :: T.NodeRuntime -> D.StateVar a -> STM a
readVar' nodeRt (view Lens.varId -> varId) = do
    nodeState <- readTMVar $ nodeRt ^. RLens.state
    case Map.lookup varId nodeState of
        Nothing                   -> error $ "Var not found: " +|| varId ||+ "."
        Just (T.VarHandle _ tvar) -> unsafeCoerce <$> readTVar tvar

writeVar' :: T.NodeRuntime -> D.StateVar a -> a -> STM ()
writeVar' nodeRt (view Lens.varId -> varId) val = do
    nodeState <- readTMVar $ nodeRt ^. RLens.state
    case Map.lookup varId nodeState of
        Nothing                   -> error $ "Var not found: " +|| varId ||+ "."
        Just (T.VarHandle _ tvar) -> writeTVar tvar $ unsafeCoerce val


-- | Interpret StateL as STM.
interpretStateL :: T.NodeRuntime -> L.StateF a -> STM a

interpretStateL nodeRt (L.NewVar  val next     ) = next . D.StateVar <$> newVar' nodeRt val

interpretStateL nodeRt (L.ReadVar var next     ) = next <$> readVar' nodeRt var

interpretStateL nodeRt (L.WriteVar var val next) = next <$> writeVar' nodeRt var val

interpretStateL _      (L.Retry _              ) = retry

interpretStateL _      (L.EvalGraph gr act next) = do
    next <$> runHGraphSTM gr act

-- | Runs state model as STM.
runStateL :: T.NodeRuntime -> L.StateL a -> STM a
runStateL nodeRt = foldFree (interpretStateL nodeRt)
