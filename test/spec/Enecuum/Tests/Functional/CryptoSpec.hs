module Enecuum.Tests.Functional.CryptoSpec where

import           Crypto.TripleSec                             (decryptIO, encryptIO)
import qualified Data.Aeson                                   as A
import           Data.ByteString.Char8                        (pack)
import qualified Data.Serialize                               as S
import           Enecuum.Core.Crypto.Crypto
import qualified Enecuum.Core.Interpreters                    as I
import qualified Enecuum.Language                             as L
import           Enecuum.Prelude                              hiding (pack)
import qualified Enecuum.Samples.Assets.Blockchain.Generation as D
import           Enecuum.Samples.Assets.Blockchain.Keys       (decryptKey, encryptKey)
import qualified Enecuum.Samples.Assets.Blockchain.Wallet     as D
import qualified Enecuum.Samples.Blockchain.Domain            as D
import qualified Enecuum.Samples.Blockchain.Language          as L
import qualified Enecuum.Samples.Blockchain.Lens              as Lens
import           Enecuum.Testing.Wrappers
import           Test.Hspec                                   (Spec, describe, shouldBe)
import           Test.Hspec.Contrib.HUnit                     (fromHUnitTest)
import           Test.HUnit                                   (Test (..))

wallets :: [KeyPair]
wallets =
    [ D.KeyPair { getPub = PublicKey256k1 162041393014266997694715179451060754359644353694678397659483656769944713491456, getPriv = PrivateKey256k1 73999854534268080627202311492113505346257058527387365056343245962475387806824}
    , D.KeyPair { getPub = PublicKey256k1 23186060215033531139909219478710866146713960681525059519120352475511495484295, getPriv = PrivateKey256k1 80275203804811449396339565643717793100181946842787928746035268781288915302925}
    , D.KeyPair { getPub = PublicKey256k1 164363796434547946853458519561021460997113148099374090547007640384559783907145, getPriv = PrivateKey256k1 45296987227396366574213327507378838872662579496436764556010122073219687535323}
    , D.KeyPair { getPub = PublicKey256k1 173043485450315606298237908613520700023796771897337701234671407559917415352139, getPriv = PrivateKey256k1 82158945569976313932019709311991001165798670590623541702268906445336500466266}
    , D.KeyPair { getPub = PublicKey256k1 147652403275775160917551599158321585505101195507923120640188760137020641154251, getPriv = PrivateKey256k1 93012031260270053609962666689128395737092805673829781984811938911430811953211}
    ]

spec :: Spec
spec = stableTest $ fastTest $ do
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
        , TestLabel "Compress/decompress public key" testCompressPublicKey]
    describe "Private key" $ fromHUnitTest $ TestList
        [ TestLabel "Private key read/show" testReadShowPrivateKey]
    describe "Encrypt/decrypt message" $ fromHUnitTest $ TestList
        [ TestLabel "Encrypt/decrypt message with password" testEncryptMsgViaPassword
        , TestLabel "Encrypt/decrypt message with password via library functions" testEncryptionWithLibraryFunctions]

test :: (Eq b, Show b) => IO b -> (b -> b) -> Test
test genFun fun = TestCase $ do
    s <- replicateM 10 genFun
    map fun s `shouldBe` s

testBool :: IO a -> (a -> Bool) -> Test
testBool genFun fun = TestCase $ do
    s <- replicateM 10 genFun
    all fun s `shouldBe` True

-- generator functions
generateTransaction :: IO D.Transaction
generateTransaction = I.runERandomL $ D.genTransaction D.Hardcoded

generateMicroblock :: IO D.Microblock
generateMicroblock = I.runERandomL $ D.genRandMicroblock D.genesisKBlock

generateSignature :: IO Signature
generateSignature = do
    tx <- I.runERandomL $ D.genTransaction D.Hardcoded
    pure $ tx ^. Lens.signature

generateKeys :: (KeyPair -> b) -> IO b
generateKeys  = (<$> (I.runERandomL $ L.evalCoreCrypto L.generateKeyPair))


generatePublicKey :: IO PublicKey
generatePublicKey = generateKeys (\(D.KeyPair pub _) -> pub)

generatePrivateKey :: IO PrivateKey
generatePrivateKey = generateKeys (\(D.KeyPair _ priv) -> priv)


-- | Signature test functions
testSignature :: (Signature -> Signature) -> Test
testSignature = test generateSignature

testSignatureBinarySerialization :: Test
testSignatureBinarySerialization = testSignature (fromRight (read "1" :: D.Signature) . S.decode . S.encode)

testReadShowSignature :: Test
testReadShowSignature = testSignature (read . show)

testSignatureJSONSerialization :: Test
testSignatureJSONSerialization = testSignature (fromJust . A.decode . A.encode)

testVerifySignedTransaction :: Test
testVerifySignedTransaction = testBool generateTransaction L.verifyTransaction

testVerifySignedMicroblock :: Test
testVerifySignedMicroblock = testBool generateMicroblock L.verifyMicroblock

-- | PublicKey test functions
testPublicKey :: (PublicKey -> PublicKey) -> Test
testPublicKey = test generatePublicKey

testReadShowPublicKey :: Test
testReadShowPublicKey = testPublicKey (read . show)

testCompressPublicKey :: Test
testCompressPublicKey = testPublicKey (D.compressPublicKey . D.decompressPublicKey)

testPublicKeyJsonSerialization :: Test
testPublicKeyJsonSerialization = testPublicKey (fromJust . A.decode . A.encode)

testPublicKeyBinarySerialization :: Test
testPublicKeyBinarySerialization = testPublicKey (fromRight (read "1" :: D.PublicKey) . S.decode . S.encode)

-- | PrivateKey test function
testReadShowPrivateKey :: Test
testReadShowPrivateKey = test generatePrivateKey (read . show)

-- | KeyPair test function
testReadShowDemoWallets :: Test
testReadShowDemoWallets = TestCase $ do
    map (read . show) wallets `shouldBe` wallets
    map (read . show) D.hardcodedWallets `shouldBe` D.hardcodedWallets

-- | Encrypt/decrypt message with password
testEncryptMsgViaPassword :: Test
testEncryptMsgViaPassword = TestCase $ do
    D.KeyPair publicKey privateKey <- runCrypto L.generateKeyPair
    let password = "secret password"
    let message = fromString $ D.showPrivateKey privateKey
    encryptMsg <- runCrypto $ encryptKey password message
    decryptedMsg <- runCrypto $ decryptKey password encryptMsg
    decryptedMsg `shouldBe` message

runCrypto s = I.runCoreEffectL undefined $ L.evalCoreCrypto s

testEncryptionWithLibraryFunctions :: Test
testEncryptionWithLibraryFunctions = TestCase $ do
  let password = "secret password" :: ByteString
  let message = "message that will be encrypted" :: ByteString
  encryptMsg <- encryptIO password message
  decryptMsg <- decryptIO password encryptMsg
  decryptMsg `shouldBe` message
