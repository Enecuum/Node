{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Enecuum.Legacy.Service.Metrics.Statsd (
  Stat,

  withRate,
  increment,
  decrement,
  count,
  gauge,
  add,
  timing,
  set
) where

import           Data.Time.Units
import           Enecuum.Legacy.Service.Types.PublicPrivateKeyPair (Amount,
                                                                    PublicKey)
import           Prelude                                           (show)
import           Text.Printf
import           Universum                                         hiding (Set,
                                                                    Type, set,
                                                                    show)

type Stat = String
data Type = Count | Gauge | Timing | Set
instance Show Type where
  show Count  = "c"
  show Gauge  = "g"
  show Timing = "ms"
  show Set    = "s"


class (Show a) => Datagram a where
   fmtDatagram :: Stat -> a -> Type -> String
   fmtDatagram stat value statType = printf "%s:%s|%s" stat (show value) (show statType)
instance Datagram Int
instance Datagram Integer
instance Datagram Double
instance Datagram Amount
instance Datagram PublicKey
instance Datagram String where
   fmtDatagram stat value statType = printf "%s:%s|%s" stat value (show statType)


withRate :: (Show a) => a -> String -> String
withRate rate msg = msg ++ "|@" ++ show rate

increment :: Stat -> String
increment stat = count stat 1

decrement :: Stat -> String
decrement stat = count stat (-1)

count :: Stat -> Integer -> String
count stat value = fmtDatagram stat value Count


class (Datagram a, Num a, Ord a) => Gauge a where
  gauge :: Stat -> a -> String
  gauge stat value = fmtDatagram stat value Gauge
  add :: Stat -> a -> String
  add stat value = fmtDatagram stat apply Gauge
             where apply = if value < 0
                           then "-" ++ show (abs value)
                           else "+" ++ show value
instance Gauge Int
instance Gauge Integer
instance Gauge Double
instance Gauge Amount

timing :: Stat -> Millisecond -> String
timing stat value = fmtDatagram stat (fromIntegral value::Integer)  Timing


class (Datagram a, Eq a) => Set a where
  set :: Stat -> a -> String
  set stat value = fmtDatagram stat value Set
instance Set Double
instance Set Integer
instance Set PublicKey
