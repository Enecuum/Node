module Enecuum.Assets.Nodes.TestServer where

import           Enecuum.Prelude

import qualified Data.Aeson                    as A
import qualified Data.Map                      as Map
import qualified Data.Text as Text
import           Control.Lens                  (makeFieldsNoPrefix)

import           Enecuum.Config                (Config)
import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import qualified Enecuum.Blockchain.Lens       as Lens
import qualified Enecuum.Framework.Lens        as Lens
import qualified Enecuum.Core.Lens             as Lens
import           Enecuum.Language              (HasGraph)

import           Enecuum.Core.HGraph.Internal.Types
import           Enecuum.Framework.Domain.RPC
import           Enecuum.Framework.RpcMethod.Language
import qualified Enecuum.Blockchain.Domain.Graph as TG
import           Enecuum.Assets.Nodes.RPC
import           Enecuum.Assets.Nodes.Address