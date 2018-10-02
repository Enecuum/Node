module Enecuum.Testing.Framework.Internal.TcpLikeServer where

import           Enecuum.Prelude

import qualified Data.Map as Map
import qualified Data.Aeson as A

import qualified Enecuum.Domain         as D
import qualified Enecuum.Language       as L
import qualified Enecuum.Framework.Lens as Lens

import qualified Enecuum.Testing.Types as T
import qualified Enecuum.Testing.RLens as RLens
import qualified Enecuum.Testing.Framework.Interpreters.Node as Impl

-- | Node RPC server worker.

startNodeTcpLikeServer nodeRt port methodVar = do
  methods <- readTVarIO methodVar
  pure ()
