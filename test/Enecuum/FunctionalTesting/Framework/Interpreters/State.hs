{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Enecuum.FunctionalTesting.Framework.Interpreters.State where

import Enecuum.Prelude

import qualified Crypto.Hash.SHA256         as SHA
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Base64     as Base64
import qualified Data.Map                   as Map
import qualified Data.Serialize             as S
import           Data.HGraph.StringHashable (StringHash (..), StringHashable, toHash)
import           Unsafe.Coerce              (unsafeCoerce)

import qualified Enecuum.Language as L
import qualified Enecuum.Domain as D
import qualified Enecuum.Framework.Lens as Lens

import qualified Enecuum.FunctionalTesting.RLens as RLens
import qualified Enecuum.FunctionalTesting.Types as T

newtype VarNumber = VarNumber Int

instance StringHashable VarNumber where
  toHash (VarNumber n) = StringHash . Base64.encode . SHA.hash $ show ("VarNumber " +|| n ||+ "" :: String)

getVarNumber :: T.NodeRuntime -> STM VarNumber
getVarNumber nodeRt = do
  number <- takeTMVar $ nodeRt ^. RLens.varCounter
  putTMVar (nodeRt ^. RLens.varCounter) $ number + 1
  pure $ VarNumber number

newVar' :: T.NodeRuntime -> a -> STM D.VarId
newVar' nodeRt a = do
  varNumber <- getVarNumber nodeRt
  tvar      <- newTVar $ unsafeCoerce a
  nodeState <- takeTMVar $ nodeRt ^. RLens.state
  let varId = D.toHash varNumber
  putTMVar (nodeRt ^. RLens.state) $ Map.insert varId (VarHandle varId tvar) nodeState
  pure varId

readVar' :: T.NodeRuntime -> D.StateVar a -> STM a
readVar' nodeRt (view Lens.varId -> varId) = do
  nodeState <- readTMVar $ nodeRt ^. RLens.state
  case Map.lookup varId nodeState of
    Nothing                 -> error $ "Var not found: " +|| varId ||+ "."
    Just (VarHandle _ tvar) -> unsafeCoerce <$> readTVar tvar

writeVar' :: T.NodeRuntime -> D.StateVar a -> a -> STM ()
writeVar' nodeRt (view Lens.varId -> varId) val = do
  nodeState <- readTMVar $ nodeRt ^. RLens.state
  case Map.lookup varId nodeState of
    Nothing                 -> error $ "Var not found: " +|| varId ||+ "."
    Just (VarHandle _ tvar) -> writeTVar tvar $ unsafeCoerce val


-- | Interpret StateL as STM.
interpretStateL :: T.NodeRuntime -> L.StateF a -> STM a

interpretStateL nodeRt (L.NewVar val next) =
  next . D.StateVar <$> newVar' nodeRt val

interpretStateL nodeRt (L.ReadVar var next) =
  next <$> readVar' nodeRt var

interpretStateL nodeRt (L.WriteVar var val next) =
  next <$> writeVar' nodeRt var val

interpretStateL nodeRt (L.EvalGraph (L.GraphAction stmRunner _ act) next) = do
  next <$> stmRunner act

-- | Runs state model as STM.
runStateL :: T.NodeRuntime -> L.StateL a -> STM a
runStateL nodeRt = foldFree (interpretStateL nodeRt)
