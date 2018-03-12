{-#Language Strict#-}
module Service.Network.WebSockets.Server (runServer) where

import              Service.Network.Base
import              Control.Concurrent
import              Control.Monad
import              Control.Exception
import              Network.WebSockets hiding (runServer)
import              Network.WebSockets.Connection
import qualified    Network.WebSockets.Stream     as Stream
import qualified    Network.Socket  as S


-- | Run a server app.
runServer ::
    HostAddress
    -> Int
    -> (HostAddress -> ServerApp)
    -> IO ()
runServer aHost aPort app = S.withSocketsDo $
  bracket
  (makeListenSocket (show aHost) aPort)
  S.close
  (\sock -> do
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
runApp socket opts app = bracket
    (makePendingConnection socket opts)
    (Stream.close . pendingStream)
    app
