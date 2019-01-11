module Enecuum.Framework.Networking.Internal.Datagram where

import           Enecuum.Prelude
import           Data.Serialize
import qualified Network.Socket.ByteString.Lazy                   as S
import qualified Network.Socket                                   as S hiding (recv, send)
import           Control.Monad.Extra

loopM :: Monad m => (a -> m (Either a b)) -> a -> m b
loopM act x = do
    res <- act x
    case res of
        Left x -> loopM act x
        Right v -> return v
    
sendDatagram :: S.Socket -> LByteString -> IO ()
sendDatagram sock msg =
    S.sendAll sock $ encodeLazy (toEnum $ length msg :: Word32) <> msg

receiveDatagram :: S.Socket -> IO LByteString
receiveDatagram sock = do
    datagramLength <- mconcat <$> replicateM 4 (S.recv sock 1)
    let len = decodeLazy datagramLength
    print len
    rawMsg <- readMsg sock len
    pure $ mconcat $ reverse rawMsg

readMsg :: S.Socket -> Either String Word32 -> IO [LByteString]
readMsg _    (Left  _  ) = pure []
readMsg sock (Right len) =
    loopM (\(elemsOfMsg, restOfMsg) -> do
        msg <- S.recv sock ((toEnum.fromEnum) restOfMsg)
        let newLen = len - toEnum (length msg)
        pure $ if newLen == 0
            then Right $ msg : elemsOfMsg
            else Left (msg : elemsOfMsg, newLen)
    ) ([], len)
