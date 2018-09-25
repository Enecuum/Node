{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}

-- TODO: this is copy-paste from tests with little changes.
module Enecuum.Assets.Nodes.NetworkNode3 where

import           Enecuum.Prelude

import qualified Data.Aeson                    as A
import qualified Data.Map                      as Map
import           Control.Lens                  (makeFieldsNoPrefix)

import           Enecuum.Config                (Config)
import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import qualified Enecuum.Blockchain.Lens       as Lens
import qualified Enecuum.Framework.Lens        as Lens
import qualified Enecuum.Core.Lens             as Lens
import qualified Data.Text as Text


import           Enecuum.Core.HGraph.Internal.Types
import           Enecuum.Legacy.Service.Network.Base (ConnectInfo (..))
import           Enecuum.Framework.Domain.RpcMessages
import           Enecuum.Framework.RpcMethod.Language
import           Enecuum.Framework.Node.Language          ( NodeL )
import           Enecuum.Blockchain.Language.Extra
import qualified Enecuum.Blockchain.Domain.Graph as TG
import           Enecuum.Assets.Nodes.RPC
import           Enecuum.Assets.Nodes.Address

data NetworkNodeChainData = NetworkNodeChainData
  {
     _chainLengthVar   :: D.StateVar Int
  }

makeFieldsNoPrefix ''NetworkNodeChainData

acceptChainLength
  :: NetworkNodeChainData
  -> GetChainLengthRequest
  -> L.NodeL GetChainLengthResponse
acceptChainLength nodeData GetChainLengthRequest =
  GetChainLengthResponse <$> (L.atomically $ L.readVar (nodeData ^. chainLengthVar))


newtorkNode3Initialization :: L.NodeL NetworkNodeChainData
newtorkNode3Initialization = do
  chainLengthVar'   <- L.atomically $ L.newVar 15
  pure $ NetworkNodeChainData chainLengthVar'

networkNode3 :: L.NodeDefinitionL ()
networkNode3 = do
  L.nodeTag "networkNode3"
  nodeData <- L.initialization $ newtorkNode3Initialization
  L.servingRpc 2003 $ 
    L.method (acceptChainLength nodeData)

