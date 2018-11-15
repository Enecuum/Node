-- TODO: this is almost exact copy-paste from testing runtime. Unify it.

module Enecuum.Core.State.Interpreter where

import Enecuum.Prelude

import qualified Data.Map             as Map
import           Unsafe.Coerce        (unsafeCoerce)

import qualified Enecuum.Core.Language   as L
import qualified Enecuum.Core.Types      as D
import qualified Enecuum.Core.Runtime    as Rt
import qualified Enecuum.Core.RLens      as RLens
import qualified Enecuum.Core.Logger.Impl.HsLogger as Impl

import           Data.HGraph.StringHashable (StringHash (..), toHash)
import           Enecuum.Core.HGraph.Interpreters.STM
import           Enecuum.Core.State.DelayedLogger (runDelayedLoggerL)

getVarNumber :: Rt.StateRuntime -> STM Rt.VarNumber
getVarNumber stateRt = do
    number <- Rt.getNextId stateRt
    pure $ Rt.VarNumber number

newVar' :: Rt.StateRuntime -> a -> STM D.VarId
newVar' stateRt a = do
    varNumber <- getVarNumber stateRt
    tvar      <- newTVar $ unsafeCoerce a
    nodeState <- takeTMVar $ stateRt ^. RLens.state
    let varId = D.toHash varNumber
    putTMVar (stateRt ^. RLens.state) $ Map.insert varId (Rt.VarHandle varId tvar) nodeState
    pure varId

readVar' :: Rt.StateRuntime -> D.StateVar a -> STM a
readVar' stateRt (D.StateVar varId) = do
    nodeState <- readTMVar $ stateRt ^. RLens.state
    case Map.lookup varId nodeState of
        Nothing                    -> error $ "Var not found: " +|| varId ||+ "."
        Just (Rt.VarHandle _ tvar) -> unsafeCoerce <$> readTVar tvar

writeVar' :: Rt.StateRuntime -> D.StateVar a -> a -> STM ()
writeVar' stateRt (D.StateVar varId) val = do
    nodeState <- readTMVar $ stateRt ^. RLens.state
    case Map.lookup varId nodeState of
        Nothing                    -> error $ "Var not found: " +|| varId ||+ "."
        Just (Rt.VarHandle _ tvar) -> writeTVar tvar $ unsafeCoerce val


-- | Interpret StateL as STM.
interpretStateL :: Rt.StateRuntime -> L.StateF a -> STM a

interpretStateL stateRt (L.NewVar  val next     ) = next . D.StateVar <$> newVar' stateRt val

interpretStateL stateRt (L.ReadVar var next     ) = next <$> readVar' stateRt var

interpretStateL stateRt (L.WriteVar var val next) = next <$> writeVar' stateRt var val

interpretStateL _       (L.Retry _              ) = retry

interpretStateL _       (L.EvalGraph gr act next) = next <$> runHGraphSTM gr act

interpretStateL stateRt (L.EvalDelayedLogger act next) = next <$> runDelayedLoggerL (stateRt ^. RLens.delayedLog) act

-- | Runs state model as STM.
runStateL :: Rt.StateRuntime -> L.StateL a -> STM a
runStateL stateRt = foldFree (interpretStateL stateRt)


-- | Writes all delayed entries into real logger.
flushDelayedLogger :: Rt.StateRuntime -> Rt.LoggerRuntime -> IO ()
flushDelayedLogger stateRt loggerRt = do
    l <- atomically $ do
            l <- readTVar $ stateRt ^. RLens.delayedLog
            writeTVar (stateRt ^. RLens.delayedLog) []
            pure l

    let loggerHandle = loggerRt ^. RLens.hsLoggerHandle
    mapM_ (\(Rt.DelayedLogEntry level msg) -> Impl.runLoggerL loggerHandle $ L.logMessage level msg) l