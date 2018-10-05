module Enecuum.Assets.Nodes.Client where

import           Enecuum.Prelude

import qualified Data.Aeson as A
import           Enecuum.Config (Config)
import qualified Enecuum.Language as L

data Msg = Msg Text 

instance A.FromJSON Msg where
    parseJSON (A.Object o) = Msg <$> o A..: "msg"

msgHandler :: Msg -> L.NodeL Text
msgHandler (Msg text) = pure text

clientNode :: L.NodeDefinitionL ()
clientNode = do
    L.logInfo "Client started"
    L.std $ do
        L.stdHandler msgHandler
