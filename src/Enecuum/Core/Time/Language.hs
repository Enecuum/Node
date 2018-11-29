module Enecuum.Core.Time.Language where

import           Data.Time             (UTCTime)
import           Data.Time.Clock.POSIX (POSIXTime)
import           Enecuum.Prelude

data TimeF next where
    GetUTCTime   :: (UTCTime   -> next) -> TimeF next
    GetPosixTime :: (POSIXTime -> next) -> TimeF next

instance Functor TimeF where
    fmap g (GetUTCTime next)   = GetUTCTime (g . next)
    fmap g (GetPosixTime next) = GetPosixTime (g . next)

type TimeL = Free TimeF

class Time m where
    getUTCTime   :: m UTCTime
    getPosixTime :: m POSIXTime

instance Time TimeL where
    getUTCTime   = liftF $ GetUTCTime   id
    getPosixTime = liftF $ GetPosixTime id
