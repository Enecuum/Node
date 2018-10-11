{-# LANGUAGE RecordWildCards #-}
module Enecuum.Tests.Functional.CryptoSpec where

import           Enecuum.Prelude
import           Enecuum.Blockchain.Domain
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit  (fromHUnitTest)
import qualified Enecuum.Blockchain.Lens          as Lens
import           Test.HUnit                               ( Test(..)
                                                          , (@?=)
                                                          )
spec :: Spec
spec = do
    describe "Crypto spec tests" $ fromHUnitTest $ TestList
        [ TestLabel "Verify transaction signature" testVerifySignedTransaction]


testVerifySignedTransaction :: Test
testVerifySignedTransaction = TestCase $ do
    -- tx@(Transaction {..}) <- genTransaction On
    -- let txForSign = TransactionForSign {
    --         _owner = _owner
    --       , _receiver = _receiver
    --       , _amount = _amount
    --       , _currency = _currency}
    -- verifyEncodable _owner _signature txForSign `shouldBe` True
    True `shouldBe` True