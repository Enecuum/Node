{-# LANGUAGE RecordWildCards #-}
module Enecuum.Tests.Functional.CryptoSpec where

import           Enecuum.Blockchain.Domain
-- import qualified Enecuum.Blockchain.Lens   as Lens
import qualified Enecuum.Core.Random.Interpreter as I
import           Enecuum.Prelude
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit  (fromHUnitTest)
import           Test.HUnit                (Test (..), (@?=))


spec :: Spec
spec = do
    describe "Crypto spec tests" $ fromHUnitTest $ TestList
        [ TestLabel "Verify transaction signature" testVerifySignedTransaction
        , TestLabel "Verify microblock signature" testVerifySignedMicroblock]


testVerifySignedTransaction :: Test
testVerifySignedTransaction = TestCase $ do
    tx@(Transaction {..}) <- I.runERandomL $ genTransaction On
    let txForSign = TransactionForSign {
            _owner = _owner
          , _receiver = _receiver
          , _amount = _amount
          , _currency = _currency}
    verifyEncodable _owner _signature txForSign `shouldBe` True

testVerifySignedMicroblock :: Test
testVerifySignedMicroblock = TestCase $ do
    mb@(Microblock {..}) <- I.runERandomL $ genRandMicroblock genesisHash
    let mbForSign = MicroblockForSign {
            _keyBlock = _keyBlock
          , _transactions = _transactions
          , _publisher = _publisher}
    verifyEncodable _publisher _signature mbForSign `shouldBe` True

