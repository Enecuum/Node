{-# LANGUAGE RecordWildCards #-}
module Enecuum.Tests.Functional.CryptoSpec where

import qualified Data.Aeson                       as A
import           Data.HGraph.StringHashable       (fromStringHash)
import qualified Data.Serialize                   as S
import qualified Enecuum.Blockchain.Domain        as D
import qualified Enecuum.Blockchain.Domain.KBlock as New
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
    describe "Crypto spec tests" $ fromHUnitTest $ TestList
        [ TestLabel "Verify transaction signature" testVerifySignedTransaction
        , TestLabel "Verify microblock signature" testVerifySignedMicroblock]
    describe "Key block hash calculation" $ fromHUnitTest $ TestList
        [ TestLabel "Hash calculation for genesis key block: legacy and new are the same" testGenesisKblockHash
        , TestLabel "Hash calculation for random key blocks: legacy and new are the same" testKblockHash]
    describe "Public key serialization" $ fromHUnitTest $ TestList
        [ TestLabel "Public key JSON serialization" testPublicKeyJsonSerialization
        , TestLabel "Public key binary serialization" testPublicKeyBinarySerialization]
    describe "Compress public key" $ fromHUnitTest $ TestList
        [ TestLabel "Compress/decompess public key" testCompressPublicKey]


generatePublicKeys = do
    keys <- replicateM 1000 $ I.runERandomL $ L.evalCoreCrypto $ L.generateKeyPair       
    let pubs = map (\(D.KeyPair pub _) -> pub) keys
    pure pubs 

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
