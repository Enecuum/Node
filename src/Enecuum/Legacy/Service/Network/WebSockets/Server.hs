{-# LANGUAGE Strict #-}

module Enecuum.Legacy.Service.Network.WebSockets.Server (runServer) where


import           Control.Concurrent
import           Control.Exception                   (allowInterrupt)
import           Control.Monad
import           Enecuum.Legacy.Node.BaseFunctions
import           Enecuum.Legacy.Service.Network.Base
import           Enecuum.Prelude
import qualified Network.Socket                      as S
import           Network.WebSockets                  hiding (runServer)
import           Network.WebSockets.Connection
import qualified Network.WebSockets.Stream           as Stream

-- | Run a server app.
runServer :: PortNumber -> String -> (HostAddress -> ServerApp) -> IO ()
runServer aPort aErrorMsg app = undead (putStrLn $ "Server will be reload:" ++ aErrorMsg) $ S.withSocketsDo $ bracket
    (makeListenSocket "0" (fromEnum aPort))
    S.close
    (\sock -> mask_ $ forever $ do
        allowInterrupt
        (conn, sockAddr) <- S.accept sock
        void
            $ forkIOWithUnmask
            $ \unmask -> finally
                  (unmask $ runApp conn defaultConnectionOptions (app (sockAddrToHostAddress sockAddr)))
                  (S.close conn)
    )


runApp :: S.Socket -> ConnectionOptions -> ServerApp -> IO ()
runApp socket opts = bracket (makePendingConnection socket opts) (Stream.close . pendingStream)
