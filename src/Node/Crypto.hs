{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
module Node.Crypto (
    makeConnectingMsg,
    makePingMsg,
    makePongMsg,
    makeInfoPing,
    makePackagedMsg,
    makeIPRequest,
    makeIHaveBroadcastConnects,
    makeInfoPingIAmPublicator,
    makeTransactionConfirmation,
    getMsgPackage,
    verifyConnectingMsg,
    verifyByteString,
    verifyIPAnswer,
    verifyIHaveBroadcastConnects,
    verifyInfoPingIAmPublicator,
    verifyTransactionConfirmation,
    genKayPair,
    encrypt,
    cryptoHash,
    fastHash
  ) where

import              Service.Network.Base (HostAddress, PortNumber)
import              Node.Data.Data
import              Node.Data.NodeTypes
import              Node.Data.NetPackage
import              Service.Types
import              System.Clock
import              Crypto.Cipher.Types
import              Crypto.Random.Types
import              Crypto.Hash
import              Crypto.PubKey.ECC.DH
import              Crypto.PubKey.ECC.ECDSA

import              Crypto.Cipher.AES   (AES256)
import              Crypto.Error        (CryptoFailable(..))
import              Data.ByteArray (unpack)

import              Data.ByteString (ByteString, pack)
import              Data.Serialize

makeConnectingMsg :: MonadRandom m =>
    MyNodeId -> PublicPoint -> PrivateKey -> PublicKey -> m PackagedMsg
makeConnectingMsg nId aPublicPoint aPrivateKey publicKey = do
    ConnectingMsg aPublicPoint (toNodeId nId) publicKey <$>
        signEncodeble aPrivateKey (aPublicPoint, toNodeId nId, publicKey)

makePingMsg :: PingPackage -> StringKey -> CryptoFailable PackagedMsg
makePingMsg = makePingPongMsg Ping

makePongMsg :: PongPackage -> StringKey -> CryptoFailable PackagedMsg
makePongMsg = makePingPongMsg Pong

makeInfoPing :: InfoPingPackage -> StringKey -> CryptoFailable PackagedMsg
makeInfoPing = makePingPongMsg InfoPing

makeIHaveBroadcastConnects ::
    Int
    -> HostAddress
    -> PortNumber
    -> MyNodeId
    -> PrivateKey
    -> IO InfoPingPackage
makeIHaveBroadcastConnects aNumOfConnects aIp aPort (MyNodeId aNodeId) aPrivateKey = do
    aTime <- getTime Realtime
    aSignature <- signEncodeble aPrivateKey
        (aTime, aNumOfConnects, aIp, aPort, NodeId aNodeId)
    return $ IHaveBroadcastConnects aTime aNumOfConnects aIp aPort
        (NodeId aNodeId) aSignature

makeTransactionConfirmation :: MonadRandom m =>
    Transaction -> MyNodeId -> PrivateKey -> m InfoPingPackage
makeTransactionConfirmation aTransaction aNodeId aPrivateKey =
    TransactionConfirmation aTransaction (toNodeId aNodeId) <$>
        signEncodeble aPrivateKey (aTransaction, toNodeId aNodeId)


makeIPRequest :: NodeId -> PrivateKey -> IO PingPackage
makeIPRequest aNodeId aPrivateKey = do
    aTime       <- getTime Realtime
    aSignature  <- signEncodeble aPrivateKey (aTime, aNodeId)
    pure $ IPRequest aTime aSignature


makeInfoPingIAmPublicator :: MonadRandom m =>
    MyNodeId -> TimeSpec -> PrivateKey -> m InfoPingPackage
makeInfoPingIAmPublicator aNodeId aTimeSpec aPrivateKey = IAmPublicator
    aTimeSpec (toNodeId aNodeId) <$> signEncodeble aPrivateKey
        (aTimeSpec, toNodeId aNodeId)

makePackagedMsg ::
    PackagedMsgStringEncoded -> StringKey -> CryptoFailable PackagedMsg
makePackagedMsg aMsg secretKey = do
    encryptedMsg <- encrypt secretKey $ toByteString aMsg
    pure $ PackagedMsg (fromByteString encryptedMsg)


getMsgPackage :: StringKey -> PackagedMsg -> CryptoFailable PackagedMsgStringEncoded
getMsgPackage secretKey (PackagedMsg aMsg) =
    fromByteString <$> encrypt secretKey (toByteString aMsg)
getMsgPackage _ _ = error "Crypto: getMsgPackage"


verifyConnectingMsg :: PackagedMsg -> Bool
verifyConnectingMsg = \case
    ConnectingMsg aPublicPoint aId aKey aSig  ->
        verifyEncodeble aKey aSig (aPublicPoint, aId, aKey) &&
        keyToId aKey == aId
    _                               -> error "Crypto: verifyConnectingMsg"


verifyInfoPingIAmPublicator :: InfoPingPackage -> Bool
verifyInfoPingIAmPublicator (IAmPublicator aTimeSpec aNodeId aSignature) =
    verifyEncodeble (idToKey aNodeId) aSignature (aTimeSpec, aNodeId)
verifyInfoPingIAmPublicator _ =
    error "verifyInfoPingIAmPublicator: it is not a \"IAmPublicator\""

verifyTransactionConfirmation :: InfoPingPackage -> Bool
verifyTransactionConfirmation
    (TransactionConfirmation aTransaction aNodeId aSignature) =
    verifyEncodeble (idToKey aNodeId) aSignature (aTransaction, aNodeId)
verifyTransactionConfirmation _ =
    error "verifyTransactionConfirmation: it is not a \"TransactionConfirmation\""


verifyIHaveBroadcastConnects :: InfoPingPackage -> Bool
verifyIHaveBroadcastConnects
    (IHaveBroadcastConnects aTime aNum aIp aPort aNodeId aSignature) =
        verifyEncodeble (idToKey aNodeId) aSignature (aTime, aNum, aIp, aPort, aNodeId)
verifyIHaveBroadcastConnects _ = error
    "verifyIHaveBroadcastConnects: it is not a \"IHaveBroadcastConnects\""


verifyIPAnswer :: NodeId -> PublicKey -> PongPackage -> Bool
verifyIPAnswer aNodeId aPublicKey (IPAnswer _ aTimeSpec aSignature) =
    verifyEncodeble aPublicKey aSignature (aTimeSpec, aNodeId)
verifyIPAnswer _ _ _ = error
    "verifyIPAnswer: it is not a \"verifyIPAnswer\"."

verifyByteString :: PublicKey -> Signature ->  ByteString -> Bool
verifyByteString = verify SHA3_256


verifyEncodeble :: Serialize msg => PublicKey -> Signature -> msg -> Bool
verifyEncodeble aPublicKey aSignature aMsg = verify SHA3_256
    aPublicKey aSignature (encode aMsg)

signEncodeble :: (MonadRandom m, Serialize msg) =>
    PrivateKey
    -> msg
    -> m Signature
signEncodeble aPrivateKey aMsg = sign aPrivateKey SHA3_256 (encode aMsg)

genKayPair :: MonadRandom m => Curve -> m (PrivateNumber, PublicPoint)
genKayPair cur = do
    aPrivateKey <- generatePrivate cur
    pure (aPrivateKey, calculatePublic cur aPrivateKey)


encrypt :: StringKey -> ByteString -> CryptoFailable ByteString
encrypt (StringKey secretKey) aMsg = do
    cipher :: AES256 <- cipherInit secretKey
    return $ ctrCombine cipher nullIV aMsg


cryptoHash :: Serialize a => a -> ByteString
cryptoHash = pack . unpack . fastHash

fastHash :: Serialize a => a -> Digest SHA3_512
fastHash bs = hash $ encode bs :: Digest SHA3_512

--------------------------------------------------------------------------------
-- INTERNAL
--------------------------------------------------------------------------------

makePingPongMsg :: Serialize a =>
    (t -> a)
    -> t
    -> StringKey
    -> CryptoFailable PackagedMsg
makePingPongMsg aCon pp key = makePackagedMsg (encodePackage (aCon pp)) key
