module Enecuum.Core.Runtime where

import Enecuum.Prelude

import qualified Data.Map                          as Map
import qualified Data.ByteString.Base64            as Base64
import qualified Crypto.Hash.SHA256                as SHA

import qualified Enecuum.Core.Types                as D
import qualified Enecuum.Core.Logger.Impl.HsLogger as Impl

newtype LoggerRuntime = LoggerRuntime
    { _hsLoggerHandle :: Maybe Impl.HsLoggerHandle
    }

data CoreRuntime = CoreRuntime
    { _loggerRuntime :: LoggerRuntime
    , _stateRuntime  :: StateRuntime
    }

newtype VarNumber = VarNumber Int

data VarHandle = VarHandle D.VarId (TVar Any)

instance D.StringHashable VarNumber where
  toHash (VarNumber n) = D.StringHash . Base64.encode . SHA.hash $ show ("VarNumber " +|| n ||+ "" :: String)

data DelayedLogEntry = DelayedLogEntry D.LogLevel D.Message
type DelayedLog = [DelayedLogEntry]

data StateRuntime = StateRuntime
    { _state      :: TMVar (Map.Map D.VarId VarHandle) -- ^ Node state.
    , _idCounter  :: TMVar Int                         -- ^ ID counter. Used to generate VarIds, ProcessIds.
    , _delayedLog :: TVar DelayedLog                   -- ^ Delayed log entries
    }

-- TODO: make it right
createVoidLoggerRuntime :: IO LoggerRuntime
createVoidLoggerRuntime = pure $ LoggerRuntime Nothing

createLoggerRuntime :: D.LoggerConfig -> IO LoggerRuntime
createLoggerRuntime config = LoggerRuntime . Just <$> Impl.setupLogger config

clearLoggerRuntime :: LoggerRuntime -> IO ()
clearLoggerRuntime (LoggerRuntime (Just hsLogger)) = Impl.teardownLogger hsLogger
clearLoggerRuntime _                               = pure ()

createStateRuntime :: IO StateRuntime
createStateRuntime = StateRuntime
    <$> newTMVarIO Map.empty
    <*> newTMVarIO 0
    <*> newTVarIO []

createCoreRuntime :: LoggerRuntime -> IO CoreRuntime
createCoreRuntime loggerRt = CoreRuntime loggerRt
    <$> createStateRuntime

clearCoreRuntime :: CoreRuntime -> IO ()
clearCoreRuntime _ = pure ()

getNextId :: StateRuntime -> STM Int
getNextId stateRt = do
    number <- takeTMVar $ _idCounter stateRt
    putTMVar (_idCounter stateRt) $ number + 1
    pure number
