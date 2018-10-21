{-# OPTIONS -fno-warn-orphans #-}

module Enecuum.Prelude
  ( module X
  ) where

import           Universum                                as X hiding ( All, Option, Set, Type, head, last, set, tail )
import           Universum.Unsafe                         as X ( head, last, tail, (!!) )
import           Universum.Functor.Fmap                   as X ( (<<$>>) )
import           Data.TypeLevel                           as X ( type (++) )
import           Data.Serialize                           as X ( Serialize )
import           Data.Aeson                               as X ( ToJSON, FromJSON, toJSON, parseJSON, genericToJSON, genericParseJSON )
import           Data.Maybe                               as X (fromJust, fromMaybe)
import           Control.Lens                             as X ( (.=), at )
import           Control.Lens.TH                          as X ( makeLenses, makeFieldsNoPrefix )
import           Control.Monad                            as X ( void, when, unless, liftM)
import           Control.Exception                        as X ( SomeException (..) )
import           GHC.Generics                             as X ( Generic )
import           Control.Newtype.Generics                 as X ( Newtype, O, pack, unpack )
import           Control.Concurrent                       as X ( ThreadId, threadDelay, forkIO )
import           Control.Concurrent.STM                   as X ( retry )
import           Control.Concurrent.STM.TVar              as X ( modifyTVar )
import           Control.Concurrent.STM.TMVar             as X ( TMVar, readTMVar, newTMVar, newTMVarIO, newEmptyTMVar, newEmptyTMVarIO, takeTMVar, putTMVar, tryReadTMVar )
import           Text.Read                                as X ( read, readsPrec )
import           GHC.Base                                 as X ( until )
import           Fmt                                      as X ( (+|), (+||), (|+), (||+) )
import           Control.Monad.Free                       as X ( Free (..), liftF, foldFree )
