module Enecuum.Framework.Networking.Internal.Datagram where

import           Enecuum.Prelude
import           Data.Serialize
import qualified Network.Socket.ByteString.Lazy                   as S
import qualified Network.Socket                                   as S hiding (recv, send)

sendDatagram :: S.Socket -> LByteString -> IO ()
sendDatagram sock msg =
    S.sendAll sock $ encodeLazy (toEnum $ length msg :: Word32) <> msg

receiveDatagram :: S.Socket -> IO LByteString
receiveDatagram sock = do
    datagramLength <- S.recv sock 4
    readMsg sock $ decodeLazy datagramLength 

readMsg :: S.Socket -> Either String Word32 -> IO LByteString 
readMsg _    (Left err)  = error $ "Decoding error " <> toText err
readMsg _    (Right 0)   = pure ""
readMsg sock (Right len) = do
    msg <- S.recv sock ((toEnum.fromEnum) len)
    (<>) <*> pure msg <$> readMsg sock (Right (len - toEnum (length msg)))