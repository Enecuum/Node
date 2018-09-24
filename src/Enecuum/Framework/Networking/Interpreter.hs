module Enecuum.Framework.Networking.Interpreter where

import Enecuum.Prelude

import           Control.Monad.Free
import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L
import qualified Enecuum.Framework.Lens             as Lens
import           Enecuum.Legacy.Service.Network.Base
import           Enecuum.Legacy.Service.Network.WebSockets.Client
import qualified Network.WebSockets                               as WS
import           Data.Aeson as A
import qualified Data.Text       as T
import           Control.Concurrent.STM.TChan (newTChan, readTChan, writeTChan)
import           Control.Exception (SomeException)
import           Enecuum.Framework.Environment

-- | Interpret NetworkingL language.
interpretNetworkingL :: L.NetworkingF RealWorld a -> IO a
interpretNetworkingL (L.OpenConnection (D.ConnectionConfig (ConnectInfo host port)) next)  = do
    chan <- atomically $ newTChan 
    var <- newEmptyMVar
    forkIO $ do
        res <- try $ runClient host (fromEnum port) "/" $ \connect -> do
            putMVar var True
            let loop = atomically (readTChan chan) >>= \case
                    D.CloseConnection        -> return ()
                    D.SendRequest request resp -> do
                        WS.sendTextData connect $ A.encode request
                        responseMsg <- WS.receiveData connect
                        let decodeMsg = (transformEither T.pack id . A.eitherDecode) responseMsg
                        atomically $ putTMVar resp decodeMsg
                        loop
            loop 
        case res of
            Left (e :: SomeException) -> putMVar var False
            Right _ -> return ()
    res <- takeMVar var
    if res
        then pure $ next (Just (D.RealWorldNetworkConnection chan))
        else pure $ next Nothing
  


interpretNetworkingL (L.CloseConnection (D.RealWorldNetworkConnection chan) next) = do
    atomically $ writeTChan chan D.CloseConnection
    pure $ next ()

interpretNetworkingL (L.SendRequest (D.RealWorldNetworkConnection chan) req next) = do
    var <- atomically $ newEmptyTMVar
    atomically $ writeTChan chan (D.SendRequest req var)
    resp <- atomically $ takeTMVar var
    pure $ next resp

interpretNetworkingL (L.EvalNetwork _ _)     = do
    error "interpretNetworkingL EvalNetwork not implemented."

interpretNetworkingL (L.SendRpcRequest (ConnectInfo host port) request next) = do
    runClient host (fromEnum port) "/" $ \connect -> do
        WS.sendTextData connect $ A.encode request
        next <$> (transformEither T.pack id . A.eitherDecode) <$> WS.receiveData connect

transformEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
transformEither f _ (Left a)  = Left (f a)
transformEither _ f (Right a) = Right (f a)

-- | Run Networking language.
runNetworkingL :: L.NetworkingL RealWorld a -> IO a
runNetworkingL = foldFree interpretNetworkingL
