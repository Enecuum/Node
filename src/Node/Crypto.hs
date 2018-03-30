{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
module Node.Crypto (
    makeConnectingMsg,
    makePackagedMsg,
    makeIPRequest,
    makeIHaveBroadcastConnects,
    makePingPongMsg,

    getMsgPackage,
    verifyConnectingMsg,
    verifyByteString,
    verifyIPAnswer,
    verifyIHaveBroadcastConnects,
    genKayPair,
    encrypt,
    cryptoHash,
    fastHash
  ) where

import              Service.Network.Base (HostAddress, PortNumber)
import              Node.Data.Data
import              Node.Data.NodeTypes
import              Node.Data.NetPackage
import              Data.Word
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

import              Sharding.Space.Point as P
import              Sharding.Types.ShardTypes

import              Data.ByteString (ByteString, pack)
import              Data.Serialize

makeConnectingMsg :: MonadRandom m =>
    MyNodeId -> PublicPoint -> PrivateKey -> PublicKey -> m PackagedMsg
makeConnectingMsg nId aPublicPoint aPrivateKey publicKey = do
    ConnectingMsg aPublicPoint (toNodeId nId) publicKey <$>
        signEncodeble aPrivateKey (aPublicPoint, toNodeId nId, publicKey)


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


makeIPRequest :: NodeId -> PrivateKey -> IO PingPackage
makeIPRequest aNodeId aPrivateKey = do
    aTime       <- getTime Realtime
    aSignature  <- signEncodeble aPrivateKey (aTime, aNodeId)
    pure $ IPRequest aTime aSignature


makePackagedMsg ::
    PackagedMsgStringEncoded -> StringKey -> CryptoFailable PackagedMsg
makePackagedMsg aMsg secretKey = do
    encryptedMsg <- encrypt secretKey $ toByteString aMsg
    pure $ PackagedMsg (fromByteString encryptedMsg)
--------------------------------------------------------------------------------
makeTheNodeHavePosition :: MyNodeId -> P.Point -> PrivateKey -> IO InfoPingPackage
makeTheNodeHavePosition aMyNodeId aPoint aPrivateKey = do
    aTime       <- getTime Realtime
    aSignature  <- signEncodeble aPrivateKey (aMyNodeId, aPoint, aTime, "TheNodeHavePosition")
    pure $ TheNodeHavePosition aMyNodeId aPoint aTime aSignature


verifyTheNodeHavePosition :: InfoPingPackage -> Bool
verifyTheNodeHavePosition = \case
    TheNodeHavePosition aMyNodeId aPoint aTimeSpec aSignature ->
        verifyEncodeble
            (idToKey $ toNodeId aMyNodeId)
            aSignature
            (aMyNodeId, aPoint, aTimeSpec, "TheNodeHavePosition")
    _   -> False


makeShardRequestPackage :: P.Point -> MyNodeId -> ShardHash -> PrivateKey -> IO RequestPackage
makeShardRequestPackage aPoint aMyNodeId aShardHash aPrivateKey = do
      aTime       <- getTime Realtime
      aSignature  <- signEncodeble aPrivateKey (aMyNodeId, aPoint, aShardHash, aTime, "ShardRequestPackage")
      pure $ ShardRequestPackage aPoint aMyNodeId aTime aShardHash aSignature


verifyShardRequestPackage :: RequestPackage -> Bool
verifyShardRequestPackage = \case
    ShardRequestPackage aPoint aMyNodeId aTime aShardHash aSignature ->
        verifyEncodeble
            (idToKey $ toNodeId aMyNodeId)
            aSignature
            (aMyNodeId, aPoint, aShardHash, aTime, "ShardRequestPackage")
    _ -> False


verifyShardAnswerPackage :: AnswerPackage -> Bool
verifyShardAnswerPackage = \case
    ShardAnswerPackage aMyNodeId aTime aShardHash aShard aSignature ->
        verifyEncodeble
            (idToKey $ toNodeId aMyNodeId)
            aSignature
            -- NOT ERROR!!! SIGNATURE TAKES FROM ShardRequestPackage --
            (aMyNodeId, aShardHash, aTime, "ShardRequestPackage")
    _ -> False

makeIamAwakeMessage :: MyNodeId -> P.Point -> PrivateKey -> IO InfoPingPackage
makeIamAwakeMessage aMyNodeId aPoint aPrivateKey = do
    aTime       <- getTime Realtime
    aSignature  <- signEncodeble aPrivateKey (aMyNodeId, aPoint, aTime, "IamAwakeMessage")
    pure $ IamAwakeMessage aMyNodeId aPoint aTime aSignature

--
verifyIamAwakeMessage :: InfoPingPackage -> Bool
verifyIamAwakeMessage = \case
    IamAwakeMessage aMyNodeId aPoint aTime aSignature ->
        verifyEncodeble
            (idToKey $ toNodeId aMyNodeId)
            aSignature
            (aMyNodeId, aPoint, aTime, "IamAwakeMessage")
    _ -> False


makeShardIndexRequestPackage :: P.Point -> NodeId -> MyNodeId -> Word64  -> PrivateKey -> IO RequestPackage
makeShardIndexRequestPackage aPoint aNodeId aMyNodeId aRadius aPrivateKey = do
    aTime       <- getTime Realtime
    aSignature  <- signEncodeble aPrivateKey (aNodeId, aPoint, aMyNodeId, aRadius, aTime, "ShardIndexRequestPackage")
    pure $ ShardIndexRequestPackage aNodeId aPoint aMyNodeId aTime aRadius aSignature


verifyShardIndexRequestPackage :: RequestPackage -> Bool
verifyShardIndexRequestPackage = \case
    ShardIndexRequestPackage aNodeId aPoint aMyNodeId aRadius aTime aSignature ->
        verifyEncodeble
            (idToKey $ toNodeId aMyNodeId)
            aSignature
            (aNodeId, aPoint, aMyNodeId, aRadius, aTime, "ShardIndexRequestPackage")
    _ -> False


makeShardRequestAdressedPackage :: P.Point -> NodeId -> MyNodeId -> ShardHash -> PrivateKey -> IO RequestPackage
makeShardRequestAdressedPackage aPoint aNodeId aMyNodeId aShardHash aPrivateKey = do
    aTime       <- getTime Realtime
    aSignature  <- signEncodeble aPrivateKey (aNodeId, aPoint, aMyNodeId, aShardHash, aTime, "ShardRequestAdressedPackage")
    pure $ ShardRequestAdressedPackage aNodeId aPoint aMyNodeId aTime aShardHash aSignature

verifyShardRequestAdressedPackage :: RequestPackage -> Bool
verifyShardRequestAdressedPackage = \case
    ShardRequestAdressedPackage aNodeId aPoint aMyNodeId aTime aShardHash aSignature ->
        verifyEncodeble
            (idToKey $ toNodeId aMyNodeId)
            aSignature
            (aNodeId, aPoint, aMyNodeId, aShardHash, aTime, "ShardRequestAdressedPackage")
    _ -> False
--------------------------------------------------------------------------------


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
