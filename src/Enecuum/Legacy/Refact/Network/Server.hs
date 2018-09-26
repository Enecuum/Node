{-# LANGUAGE Strict #-}

module Enecuum.Legacy.Refact.Network.Server (runServer, ServerComand (..)) where


import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception                   (allowInterrupt)
import           Control.Monad
import           Enecuum.Legacy.Service.Network.Base
import           Enecuum.Prelude
import qualified Network.Socket                      as S
import           Network.WebSockets                  hiding (runServer)
import           Network.WebSockets.Connection
import qualified Network.WebSockets.Stream           as Stream
import    Control.Concurrent.STM.TChan

data ServerComand = StopServer

runServer :: TChan ServerComand -> PortNumber -> (HostAddress -> ServerApp) -> IO ()
runServer chan port app =
    void $ race (void $ atomically $ readTChan chan) (runServer' port app)

-- | Run a server app.
runServer' :: PortNumber -> (HostAddress -> ServerApp) -> IO ()
runServer' port app = S.withSocketsDo $ bracket
    (makeListenSocket "0" (fromEnum port))
    S.close
    (\sock ->
        mask_ $ forever $ do
        allowInterrupt
        (conn, sockAddr) <- S.accept sock
        void $ forkIOWithUnmask $ \unmask ->
            finally (unmask $ runApp conn defaultConnectionOptions
                (app (sockAddrToHostAddress sockAddr))) (S.close conn)
        )


runApp ::
    S.Socket
    -> ConnectionOptions
    -> ServerApp
    -> IO ()
runApp socket opts = bracket
    (makePendingConnection socket opts)
    (Stream.close . pendingStream)
