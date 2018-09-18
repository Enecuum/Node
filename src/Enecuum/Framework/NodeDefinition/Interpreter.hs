module Enecuum.Framework.NodeDefinition.Interpreter where

--
import Enecuum.Prelude

import           Eff                                ( handleRelay )

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L
import qualified Network.WebSockets                 as WS


import Control.Monad.Free
import Enecuum.Framework.Node.Interpreter
import Enecuum.Framework.RpcMethod.Interpreter
import qualified Data.Map as M
import           Data.Aeson as A
import           Enecuum.Framework.RpcMethod.Language as L
import           Enecuum.Framework.Node.Language
import           Enecuum.Core.Logger.Interpreter
import           Enecuum.Legacy.Service.Network.WebSockets.Server  (runServer)


interpretNodeDefinitionL :: L.NodeDefinitionF a -> IO a
interpretNodeDefinitionL (L.NodeTag tag next) = do
    L.logInfo $ "Node tag: " +| tag |+ ""
    pure $ next ()

interpretNodeDefinitionL (L.EvalNodeModel initScript next) = do
    L.logInfo "EvalNodeModel"
    next <$> runNodeModel initScript

interpretNodeDefinitionL (L.Serving handlersF next) = do
    L.logInfo "Function serving is undefined"
    undefined

interpretNodeDefinitionL (L.ServingRpc initScript next) = do
    L.logInfo "Start of servingRpc"
    m <- atomically $ newTVar mempty
    a <- runRpcMethodL m initScript
    void $ forkIO $ runRpcServer runNodeModel m
    return $ next a


runRpcServer runner methodVar = do
    methods <- readTVarIO methodVar
    runServer 1666 "/" $ \_ pending -> do
        connect     <- WS.acceptRequest pending
        msg         <- WS.receiveData connect
        response    <- callRpc runner methods msg
        WS.sendTextData connect $ A.encode response

data RpcRequest  = RpcRequest  Text A.Value Int

instance FromJSON RpcRequest where
    parseJSON (Object o) = RpcRequest
        <$> o .: "method"
        <*> o .: "params"
        <*> o .: "id"
    parseJSON _ = error ""

instance ToJSON RpcRequest where
    toJSON (RpcRequest method val requesId) = object [
        "method" A..= method,
        "params" A..= val,
        "id"     A..= requesId 
      ]

callRpc runner methods msg = case A.decodeStrict msg of
    Just (RpcRequest method params reqId) -> case method `M.lookup` methods of
        Just justMethod -> runner $ justMethod params reqId
        Nothing -> return $ L.RpcResponseError
            (String $ "The method " <> method <> " is'nt supported.")
            reqId
    Nothing -> return $ L.RpcResponseError (String "error of request parsing") 0


runNodeDefinitionL = foldFree interpretNodeDefinitionL
