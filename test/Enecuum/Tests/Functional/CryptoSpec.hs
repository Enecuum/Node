{-# LANGUAGE RecordWildCards #-}
module Enecuum.Tests.Functional.CryptoSpec where

import qualified Data.Aeson                           as A
import           Data.HGraph.StringHashable           (fromStringHash)
import qualified Data.Serialize                       as S
import qualified Enecuum.Assets.Blockchain.Generation as D
import qualified Enecuum.Assets.Blockchain.Wallet     as D
import qualified Enecuum.Blockchain.Domain            as D
import           Enecuum.Blockchain.Domain.Crypto
import qualified Enecuum.Blockchain.Language          as L
import qualified Enecuum.Blockchain.Lens              as Lens
import qualified Enecuum.Core.Interpreters            as I
import qualified Enecuum.Language                     as L
import           Enecuum.Prelude
import           Test.Hspec                           (Spec, describe, shouldBe)
import           Test.Hspec.Contrib.HUnit             (fromHUnitTest)
import           Test.HUnit                           (Test (..))

wallets =
    [ D.KeyPair { getPub = PublicKey256k1 162041393014266997694715179451060754359644353694678397659483656769944713491456, getPriv = PrivateKey256k1 73999854534268080627202311492113505346257058527387365056343245962475387806824}
    , D.KeyPair { getPub = PublicKey256k1 23186060215033531139909219478710866146713960681525059519120352475511495484295, getPriv = PrivateKey256k1 80275203804811449396339565643717793100181946842787928746035268781288915302925}
    , D.KeyPair { getPub = PublicKey256k1 164363796434547946853458519561021460997113148099374090547007640384559783907145, getPriv = PrivateKey256k1 45296987227396366574213327507378838872662579496436764556010122073219687535323}
    , D.KeyPair { getPub = PublicKey256k1 173043485450315606298237908613520700023796771897337701234671407559917415352139, getPriv = PrivateKey256k1 82158945569976313932019709311991001165798670590623541702268906445336500466266}
    , D.KeyPair { getPub = PublicKey256k1 147652403275775160917551599158321585505101195507923120640188760137020641154251, getPriv = PrivateKey256k1 93012031260270053609962666689128395737092805673829781984811938911430811953211}
    ]

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
generateTransaction = I.runERandomL $ D.genTransaction D.Hardcoded
generateMicroblock = I.runERandomL $ D.genRandMicroblock D.genesisKBlock
generateSignature = do
    tx <- I.runERandomL $ D.genTransaction D.Hardcoded
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
testVerifySignedTransaction = testBool generateTransaction L.verifyTransaction
testVerifySignedMicroblock = testBool generateMicroblock L.verifyMicroblock

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
    map (read . show) wallets `shouldBe` wallets
    map (read . show) D.hardcodedWallets `shouldBe` D.hardcodedWallets


