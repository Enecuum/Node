{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Enecuum.Framework.Testing.Node.Interpreters.State where

import Enecuum.Prelude

import           Control.Monad.Free                                     (foldFree)
import qualified Data.Map                                               as Map
import           Unsafe.Coerce                                          (unsafeCoerce)

import qualified Enecuum.Language                                       as L
import qualified Enecuum.Domain                                         as D
import           Enecuum.Core.HGraph.Interpreter                        (runHGraph)

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

newVar' :: NodeRuntime -> Any -> STM D.VarId
newVar' nodeRt any = do
  varNumber <- getVarNumber nodeRt
  tvar      <- newTVar any
  nodeState <- takeTMVar $ nodeRt ^. RLens.state
  let varId = D.toHash varNumber
  putTMVar (nodeRt ^. RLens.state) $ Map.insert varId (VarHandle varId tvar) nodeState
  pure varId


-- | Interpret StateL as STM.
interpretStateL :: NodeRuntime -> L.StateF a -> STM a

interpretStateL nodeRt (L.NewVar val next) = do
  varId <- newVar' nodeRt $ unsafeCoerce val
  pure $ next $ D.StateVar varId

interpretStateL nodeRt (L.ReadVar var next) =
  -- next <$> readVar' var
  error "ReadVar not implemented"

interpretStateL nodeRt (L.WriteVar var val next) =
  -- next <$> writeVar' var val
  error "WriteVar not implemented"

interpretStateL nodeRt (L.EvalGraphStateF graphAction next) = do
  error "Eval graph not implemented."
  -- Impl.runLoggerL (nodeRt ^. RLens.loggerRuntime) $ L.logInfo "L.EvalGraph"
  -- next <$> runHGraph (nodeRt ^. RLens.graph) graphAction

-- | Runs state model as STM.
runStateL :: NodeRuntime -> L.StateL a -> STM a
runStateL nodeRt = foldFree (interpretStateL nodeRt)
