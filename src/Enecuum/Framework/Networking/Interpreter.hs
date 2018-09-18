module Enecuum.Framework.Networking.Interpreter where

import Enecuum.Prelude

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L
import qualified Enecuum.Framework.Lens             as Lens



-- | Interpret NetworkingL language.
--interpretNetworkingL :: L.NetworkingL a -> Eff L.NetworkModel a
interpretNetworkingL (L.OpenConnection _ _)  = do
    L.logInfo "OpenConnection cfg"
    undefined

interpretNetworkingL (L.CloseConnection _ _) = do
    L.logInfo "CloseConnection conn"
    undefined

interpretNetworkingL (L.SendRequest _ _ _)   = do
    L.logInfo "SendRequest conn req"
    undefined

interpretNetworkingL (L.EvalNetwork _ _)     = do
    L.logInfo "Eval Network"
    undefined