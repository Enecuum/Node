{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Enecuum.Framework.Testing.Node.Interpreters.State where

import Enecuum.Prelude

import           Control.Monad.Free                                     (foldFree)
import qualified Data.Map                                               as Map
import           Unsafe.Coerce                                          (unsafeCoerce)

import qualified Enecuum.Language                                       as L
import qualified Enecuum.Domain                                         as D
import qualified Enecuum.Framework.Lens                                 as Lens
import           Enecuum.Core.HGraph.Interpreters.STM                   (runHGraphSTM)

import qualified Enecuum.Core.Testing.Runtime.Interpreters              as Impl
import           Enecuum.Framework.Testing.Types
import qualified Enecuum.Framework.Testing.Lens                         as RLens
import qualified Enecuum.Framework.Testing.Node.Interpreters.Networking as Impl

import qualified Data.ByteString.Base64  as Base64
import qualified Data.Serialize          as S
import qualified Data.Aeson              as A
import qualified Crypto.Hash.SHA256      as SHA
import           Data.HGraph.StringHashable (StringHash (..), StringHashable, toHash)

newtype VarNumber = VarNumber Int

instance StringHashable VarNumber where
  toHash (VarNumber n) = StringHash . Base64.encode . SHA.hash $ show ("VarNumber " +|| n ||+ "" :: String)

getVarNumber :: NodeRuntime -> STM VarNumber
getVarNumber nodeRt = do
  number <- takeTMVar $ nodeRt ^. RLens.varCounter
  putTMVar (nodeRt ^. RLens.varCounter) $ number + 1
  pure $ VarNumber number

newVar' :: NodeRuntime -> a -> STM D.VarId
newVar' nodeRt a = do
  varNumber <- getVarNumber nodeRt
  tvar      <- newTVar $ unsafeCoerce a
  nodeState <- takeTMVar $ nodeRt ^. RLens.state
  let varId = D.toHash varNumber
  putTMVar (nodeRt ^. RLens.state) $ Map.insert varId (VarHandle varId tvar) nodeState
  pure varId

readVar' :: NodeRuntime -> D.StateVar a -> STM a
readVar' nodeRt (view Lens.varId -> varId) = do
  nodeState <- readTMVar $ nodeRt ^. RLens.state
  case Map.lookup varId nodeState of
    Nothing                 -> error $ "Var not found: " +|| varId ||+ "."
    Just (VarHandle _ tvar) -> unsafeCoerce <$> readTVar tvar

writeVar' :: NodeRuntime -> D.StateVar a -> a -> STM ()
writeVar' nodeRt (view Lens.varId -> varId) val = do
  nodeState <- readTMVar $ nodeRt ^. RLens.state
  case Map.lookup varId nodeState of
    Nothing                 -> error $ "Var not found: " +|| varId ||+ "."
    Just (VarHandle _ tvar) -> writeTVar tvar $ unsafeCoerce val


-- | Interpret StateL as STM.
interpretStateL :: NodeRuntime -> L.StateF a -> STM a

interpretStateL nodeRt (L.NewVar val next) =
  next . D.StateVar <$> newVar' nodeRt val

interpretStateL nodeRt (L.ReadVar var next) =
  next <$> readVar' nodeRt var

interpretStateL nodeRt (L.WriteVar var val next) =
  next <$> writeVar' nodeRt var val

interpretStateL nodeRt (L.EvalGraph graphAction next) = do
  next <$> runHGraphSTM (nodeRt ^. RLens.graph) graphAction

-- | Runs state model as STM.
runStateL :: NodeRuntime -> L.StateL a -> STM a
runStateL nodeRt = foldFree (interpretStateL nodeRt)
