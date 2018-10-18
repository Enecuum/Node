{-# LANGUAGE RecordWildCards #-}
module Enecuum.Tests.Functional.CryptoSpec where

import qualified Data.Aeson                      as A
import           Data.HGraph.StringHashable      (fromStringHash)
import qualified Data.Serialize                  as S
import qualified Enecuum.Assets.Blockchain.Generation as D
import qualified Enecuum.Assets.Blockchain.Wallet     as D (wallets1, wallets2)
import qualified Enecuum.Blockchain.Domain       as D
import qualified Enecuum.Blockchain.Lens         as Lens
import qualified Enecuum.Core.Interpreters       as I
import qualified Enecuum.Language                as L
import           Enecuum.Prelude
import           Test.Hspec                      (Spec, describe, shouldBe)
import           Test.Hspec.Contrib.HUnit        (fromHUnitTest)
import           Test.HUnit                      (Test (..))

spec :: Spec
spec = do
    describe "Signature" $ fromHUnitTest $ TestList
        [ TestLabel "Verify transaction signature" testVerifySignedTransaction
        , TestLabel "Verify microblock signature" testVerifySignedMicroblock
        , TestLabel "Signature: read/show" testReadShowSignature
        , TestLabel "Signature: binary serialization" testSignatureBinarySerialization
        , TestLabel "Signature: JSON serialization" testSignatureJSONSerialization]
    describe "Public key" $ fromHUnitTest $ TestList
        [ TestLabel "Public key JSON serialization" testPublicKeyJsonSerialization
        , TestLabel "Public key binary serialization" testPublicKeyBinarySerialization
        , TestLabel "Public key read/show" testReadShowPublicKey
        , TestLabel "Wallets demo read/show" testReadShowDemoWallets
        , TestLabel "Compress/decompess public key" testCompressPublicKey]
    describe "Private key" $ fromHUnitTest $ TestList
        [ TestLabel "Private key read/show" testReadShowPrivateKey]

test genFun fun = TestCase $ do
    s <- replicateM 10 genFun
    map fun s `shouldBe` s

testBool :: IO a -> (a -> Bool) -> Test
testBool genFun fun = TestCase $ do
    s <- replicateM 10 genFun
    and (map fun s) `shouldBe` True

-- generator functions
generateTransaction = I.runERandomL $ D.genTransaction D.On
generateMicroblock = I.runERandomL $ D.genRandMicroblock D.genesisKBlock
generateSignature = do
    tx <- I.runERandomL $ D.genTransaction D.On
    pure $ tx ^. Lens.signature

generateKeys fun = do
    keys <- I.runERandomL $ L.evalCoreCrypto $ L.generateKeyPair
    pure $ fun keys
generatePublicKey = generateKeys (\(D.KeyPair pub _) -> pub)
generatePrivateKey = generateKeys (\(D.KeyPair _ priv) -> priv)


-- | Signature test functions
testSignature fun = test generateSignature fun

testSignatureBinarySerialization = testSignature (\s -> fromRight (read "1" :: D.Signature) $ S.decode $ S.encode s)
testReadShowSignature = testSignature (read . show)
testSignatureJSONSerialization = testSignature (fromJust . A.decode . A.encode)
testVerifySignedTransaction = testBool generateTransaction D.verifyTransaction
testVerifySignedMicroblock = testBool generateMicroblock D.verifyMicroblock

-- | PublicKey test functions
testPublicKey fun = test generatePublicKey fun

testReadShowPublicKey = testPublicKey (read . show)
testCompressPublicKey = testPublicKey (D.compressPublicKey . D.decompressPublicKey)
testPublicKeyJsonSerialization = testPublicKey (fromJust . A.decode . A.encode)
testPublicKeyBinarySerialization = testPublicKey (\s -> fromRight (read "1" :: D.PublicKey) $ S.decode $ S.encode s)

-- | PrivateKey test function
testReadShowPrivateKey :: Test
testReadShowPrivateKey = test generatePrivateKey (read . show)

-- | KeyPair test function
testReadShowDemoWallets :: Test
testReadShowDemoWallets = TestCase $ do
    map (read . show) D.wallets1 `shouldBe` D.wallets1
    map (read . show) D.wallets2 `shouldBe` D.wallets2


