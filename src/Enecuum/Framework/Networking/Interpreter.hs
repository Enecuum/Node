module Enecuum.Framework.Networking.Interpreter where

import Enecuum.Prelude

import           Data.Aeson as A
import qualified Data.Text as T

import qualified Enecuum.Domain                     as D
import qualified Enecuum.Language                   as L
import           Enecuum.Legacy.Service.Network.WebSockets.Client
import qualified Network.WebSockets as WS
import           Control.Concurrent.STM.TChan (newTChan, readTChan, writeTChan)

-- instance D.ConnectionClass D.RealConnection where
--     openConnection (D.Address host port) = do
--         chan <- atomically newTChan
--         var <- newEmptyMVar
--         forkIO $ do
--             res <- try $ runClient host (fromEnum port) "/" $ \connect -> do
--                 putMVar var True
--                 let loop = atomically (readTChan chan) >>= \case
--                         D.CloseConnection        -> return ()
--                         D.SendRequest request resp -> do
--                             WS.sendTextData connect $ A.encode request
--                             responseMsg <- WS.receiveData connect
--                             let decodeMsg = (transformEither T.pack id . A.eitherDecode) responseMsg
--                             atomically $ putTMVar resp decodeMsg
--                             loop
--                 loop
--             case res of
--                 Left (_ :: SomeException) -> putMVar var False
--                 Right _ -> return ()
--         res <- takeMVar var
--         if res
--             then pure (Just (D.RealConnection chan))
--             else pure Nothing

    -- :: Monad m => a -> m ()
    -- closeConnection (D.RealConnection chan) = atomically $ writeTChan chan D.CloseConnection
    -- sendRequest (D.RealConnection chan) req = do
    --     var <- atomically newEmptyTMVar
    --     atomically $ writeTChan chan (D.SendRequest req var)
    --     atomically $ takeTMVar var

-- | Interpret NetworkingL language.
interpretNetworkingL :: L.NetworkingF a -> IO a
interpretNetworkingL (L.OpenConnection address next) =
    error "OpenConnection not implemented."
    -- conn :: Maybe D.RealConnection <- D.openConnection cfg
    -- pure $ next $ D.NetworkConnection <$> conn

interpretNetworkingL (L.CloseConnection D.NetworkConnection next) =
    error "CloseConnection not implemented."
    -- D.closeConnection conn >> pure (next ())

-- interpretNetworkingL (L.Send (D.NetworkConnection conn) req next) =
--     next <$> D.sendRequest conn req

interpretNetworkingL (L.EvalNetwork _ _) =
    error "interpretNetworkingL EvalNetwork not implemented."

interpretNetworkingL (L.SendRpcRequest (D.Address host port) request next) =
    runClient host (fromEnum port) "/" $ \connect -> do
        WS.sendTextData connect $ A.encode request
        next . transformEither T.pack identity . A.eitherDecode <$> WS.receiveData connect

transformEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
transformEither f _ (Left a)  = Left (f a)
transformEither _ f (Right a) = Right (f a)

-- | Run Networking language.
runNetworkingL :: L.NetworkingL a -> IO a
runNetworkingL = foldFree interpretNetworkingL
