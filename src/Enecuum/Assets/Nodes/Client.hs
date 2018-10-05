module Enecuum.Assets.Nodes.Client where

import           Enecuum.Prelude

import qualified Enecuum.Domain as D
import qualified Data.Aeson as A
import qualified Enecuum.Language as L
import qualified Enecuum.Assets.Nodes.Messages as M
import           Enecuum.Assets.Nodes.Address

data GetLastKBlock    = GetLastKBlock
newtype GetWalletBalance = GetWalletBalance M.WalletId

instance A.FromJSON GetWalletBalance where
    parseJSON (A.Object o) = GetWalletBalance <$> o A..: "walletID"

instance A.FromJSON GetLastKBlock where
    parseJSON _ = pure GetLastKBlock

getLastKBlockHandler :: GetLastKBlock -> L.NodeL Text
getLastKBlockHandler _ = do
    res :: Either Text D.KBlock <-
        L.makeRpcRequest graphNodeAddr M.GetLastKBlock
    pure . eitherToText $ res

getWalletBalance :: GetWalletBalance -> L.NodeL Text
getWalletBalance (GetWalletBalance walletId) = do
    res :: Either Text M.WalletBalanceMsg <-
        L.makeRpcRequest graphNodeAddr (M.GetWalletBalance walletId)
    pure . eitherToText $ res

{-
Requests:
    {"method":"GetLastKBlock"}
    {"method":"GetWalletBalance", "walletID":"1234"}
-}

clientNode :: L.NodeDefinitionL ()
clientNode = do
    L.logInfo "Client started"
    L.std $ do
        L.stdHandler getLastKBlockHandler
        L.stdHandler getWalletBalance

eitherToText :: Show a => Either Text a -> Text
eitherToText (Left a)  = a
eitherToText (Right a) = show a