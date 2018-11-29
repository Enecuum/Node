module Enecuum.Core.Time.Interpreter where

import           Data.Time                  (getCurrentTime)
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import qualified Enecuum.Core.Time.Language as L
import           Enecuum.Prelude

interpretTimeF :: L.TimeF a -> IO a
interpretTimeF (L.GetUTCTime   next) = next <$> getCurrentTime
interpretTimeF (L.GetPosixTime next) = next <$> getPOSIXTime

runTimeL :: Free L.TimeF a -> IO a
runTimeL = foldFree interpretTimeF
