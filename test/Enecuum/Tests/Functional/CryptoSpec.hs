{-# LANGUAGE RecordWildCards #-}
module Enecuum.Tests.Functional.CryptoSpec where

import qualified Data.Aeson                       as A
import           Data.HGraph.StringHashable       (fromStringHash)
import qualified Data.Serialize                   as S
import qualified Enecuum.Assets.Nodes.Wallet      as D (wallets1)
import qualified Enecuum.Blockchain.Domain        as D
import qualified Enecuum.Blockchain.Domain.KBlock as New
import qualified Enecuum.Blockchain.Lens          as Lens
import qualified Enecuum.Core.Interpreters        as I
import qualified Enecuum.Language                 as L
import qualified Enecuum.Legacy.Refact.Assets     as Old (genesisKeyBlock)
import qualified Enecuum.Legacy.Refact.Hashing    as Old (calculateKeyBlockHash)
import qualified Enecuum.Legacy.Service.Types     as Old (KeyBlockInfoPoW (..))
import           Enecuum.Prelude
import           Test.Hspec                       (Spec, describe, shouldBe)
import           Test.Hspec.Contrib.HUnit         (fromHUnitTest)
import           Test.HUnit                       (Test (..))


spec :: Spec
spec = do
    describe "Signature" $ fromHUnitTest $ TestList
        [ TestLabel "Verify transaction signature" testVerifySignedTransaction
        , TestLabel "Verify microblock signature" testVerifySignedMicroblock
        , TestLabel "Signature: read/show" testReadShowSignature]
    -- describe "Transaction" $ fromHUnitTest $ TestList
    --     [ TestLabel "Read/show transaction" testReadShowTransaction ]
    describe "Key block hash calculation" $ fromHUnitTest $ TestList
        [ TestLabel "Hash calculation for genesis key block: legacy and new are the same" testGenesisKblockHash
        , TestLabel "Hash calculation for random key blocks: legacy and new are the same" testKblockHash]
    describe "Public key" $ fromHUnitTest $ TestList
        [ TestLabel "Public key JSON serialization" testPublicKeyJsonSerialization
        , TestLabel "Public key binary serialization" testPublicKeyBinarySerialization
        , TestLabel "Public key read/show" testReadShowPublicKey
        , TestLabel "Wallets demo read/show" testReadShowDemoWallets
        , TestLabel "Compress/decompess public key" testCompressPublicKey]
    describe "Private key" $ fromHUnitTest $ TestList
        [ TestLabel "Private key read/show" testReadShowPrivateKey]

generatePublicKeys = generateKeys (\(D.KeyPair pub _) -> pub)
generatePrivateKeys = generateKeys (\(D.KeyPair _ priv) -> priv)

generateKeys fun = do
    keys <- replicateM 1000 $ I.runERandomL $ L.evalCoreCrypto $ L.generateKeyPair
    let pubs = map fun keys
    pure pubs

testReadShowDemoWallets :: Test
testReadShowDemoWallets = TestCase $ do
    -- map (read . show) D.publicKeys1 `shouldBe` D.publicKeys1
    -- map (read . show) D.privateKeys1 `shouldBe` D.privateKeys1
    map (read . show) D.wallets1 `shouldBe` D.wallets1

testReadShowSignature :: Test
testReadShowSignature = TestCase $ do
    tx <- replicateM 1 $ I.runERandomL $ D.genTransaction D.Off
    -- tx <- replicateM 1 genTransaction
    let signatures = map (\t -> t ^. Lens.signature) tx
    map (read . show) signatures `shouldBe` signatures

-- testReadShowTransaction :: Test
-- testReadShowTransaction = TestCase $ do
--     tx <- replicateM 1000 $ I.runERandomL $ D.genTransaction D.On
--     map (read . show) tx `shouldBe` tx

testReadShowPrivateKey :: Test
testReadShowPrivateKey = TestCase $ do
    keys <- generatePrivateKeys
    map (read . show) keys `shouldBe` keys

testReadShowPublicKey :: Test
testReadShowPublicKey = TestCase $ do
    pubs <- generatePublicKeys
    map (read . show) pubs `shouldBe` pubs

testCompressPublicKey :: Test
testCompressPublicKey = TestCase $ do
    pubs <- generatePublicKeys
    map (D.compressPublicKey . D.decompressPublicKey) pubs `shouldBe` pubs

testPublicKeyJsonSerialization :: Test
testPublicKeyJsonSerialization = TestCase $ do
    pubs <- generatePublicKeys
    map (A.decode . A.encode) pubs `shouldBe` map Just pubs

testPublicKeyBinarySerialization :: Test
testPublicKeyBinarySerialization = TestCase $ do
    pubs <- generatePublicKeys
    map (S.decode . S.encode) pubs `shouldBe` map Right pubs

testVerifySignedTransaction :: Test
testVerifySignedTransaction = TestCase $ do
    t <- I.runERandomL $ D.genTransaction D.On
    D.verifyTransaction t `shouldBe` True

testVerifySignedMicroblock :: Test
testVerifySignedMicroblock = TestCase $ do
    mb <- I.runERandomL $ D.genRandMicroblock D.genesisKBlock
    D.verifyMicroblock mb `shouldBe` True

testGenesisKblockHash :: Test
testGenesisKblockHash = TestCase $ do
    let new = New.calculateKeyBlockHash New.genesisKBlock
        old = Old.calculateKeyBlockHash Old.genesisKeyBlock
    new `shouldBe` old

newKBlockToOld :: D.KBlock -> Old.KeyBlockInfoPoW
newKBlockToOld D.KBlock{..} = Old.KeyBlockInfoPoW {
    _time      = _time
  , _prev_hash = fromStringHash _prevHash
  , _number    = _number
  , _nonce     = _nonce
  , _solver    = fromStringHash _solver
  , _type      = New.kBlockType
}

testKblockHash :: Test
testKblockHash = TestCase $ do
    k <- replicateM 1000 (I.runERandomL D.genRandKeyBlock)
    let new = map New.calculateKeyBlockHash k
        old = map (Old.calculateKeyBlockHash . newKBlockToOld) k
    new `shouldBe` old
