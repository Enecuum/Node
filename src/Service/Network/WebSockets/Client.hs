{-# Language Strict, LambdaCase, ScopedTypeVariables #-}
module Service.Network.WebSockets.Client (runClient) where

import              Network.WebSockets hiding (runClientWith, runClient)
import qualified    Network.Socket                as S
import              Control.Exception


runClient :: String       -- ^ Host
          -> Int          -- ^ Port
          -> String       -- ^ Path
          -> ClientApp a  -- ^ Client application
          -> IO a
runClient host port path =
    runClientWith host port path defaultConnectionOptions []


runClientWith :: String             -- ^ Host
              -> Int                -- ^ Port
              -> String             -- ^ Path
              -> ConnectionOptions  -- ^ Options
              -> Headers            -- ^ Custom headers to send
              -> ClientApp a        -- ^ Client application
              -> IO a
runClientWith host port path0 opts customHeaders app = do
    -- Create and connect socket
    let hints = S.defaultHints
                    {S.addrSocketType = S.Stream}

        -- Correct host and path.
        fullHost = if port == 80 then host else host ++ ":" ++ show port
        path     = if null path0 then "/" else path0
    addr:_ <- S.getAddrInfo (Just hints) (Just host) (Just $ show port)
    sock      <- S.socket (S.addrFamily addr) S.Stream S.defaultProtocol
    S.setSocketOption sock S.NoDelay 1

    -- Connect WebSocket and run client
    onException (S.connect sock (S.addrAddress addr) >>
         runClientWithSocket sock fullHost path opts customHeaders app)
                (S.close sock >> (putStrLn $ "WebSocket exception on " ++ host ++ ":" ++ show port))
