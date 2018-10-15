{-# LANGUAGE RecordWildCards #-}
module Enecuum.Tests.Functional.CryptoSpec where

import           Data.HGraph.StringHashable          (fromStringHash)
import           Enecuum.Blockchain.Domain
import qualified Enecuum.Blockchain.Domain.KBlock    as New
import qualified Enecuum.Core.Random.Interpreter     as I
import qualified Enecuum.Language                    as L
import qualified Enecuum.Legacy.Refact.Assets        as Old (genesisKeyBlock)
import qualified Enecuum.Legacy.Refact.Hashing       as Old (calculateKeyBlockHash)
import qualified Enecuum.Legacy.Service.Types        as Old (KeyBlockInfoPoW (..))
import           Enecuum.Prelude
import           Test.Hspec                          (Spec, describe, shouldBe)
import           Test.Hspec.Contrib.HUnit            (fromHUnitTest)
import           Test.HUnit                          (Test (..))

spec :: Spec
spec = do
    describe "Crypto spec tests" $ fromHUnitTest $ TestList
        [ TestLabel "Verify transaction signature" testVerifySignedTransaction
        , TestLabel "Verify microblock signature" testVerifySignedMicroblock]
    describe "Key block hash calculation" $ fromHUnitTest $ TestList    
        [ TestLabel "Hash calculation for genesis key block: legacy and new are the same" testGenesisKblockHash
        , TestLabel "Hash calculation for random key blocks: legacy and new are the same" testKblockHash]

testVerifySignedTransaction :: Test
testVerifySignedTransaction = TestCase $ do
    t <- I.runERandomL $ genTransaction On
    verifyTransaction t `shouldBe` True

testVerifySignedMicroblock :: Test
testVerifySignedMicroblock = TestCase $ do
    mb <- I.runERandomL $ genRandMicroblock genesisKBlock
    verifyMicroblock mb `shouldBe` True

testGenesisKblockHash :: Test
testGenesisKblockHash = TestCase $ do
    let new = New.calculateKeyBlockHash New.genesisKBlock
        old = Old.calculateKeyBlockHash Old.genesisKeyBlock
    new `shouldBe` old

newKBlockToOld :: KBlock -> Old.KeyBlockInfoPoW
newKBlockToOld KBlock{..} = Old.KeyBlockInfoPoW {
    _time      = _time
  , _prev_hash = fromStringHash _prevHash
  , _number    = _number
  , _nonce     = _nonce
  , _solver    = fromStringHash _solver
  , _type      = New.kBlockType
}

testKblockHash :: Test
testKblockHash = TestCase $ do
    k <- replicateM 1000 (I.runERandomL genRandKeyBlock)
    let new = map New.calculateKeyBlockHash k
        old = map (Old.calculateKeyBlockHash . newKBlockToOld) k
    new `shouldBe` old
