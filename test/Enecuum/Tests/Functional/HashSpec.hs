{-# LANGUAGE RecordWildCards #-}
module Enecuum.Tests.Functional.HashSpec where

import           Data.HGraph.StringHashable           (fromStringHash)
import qualified Enecuum.Assets.Blockchain.Generation as D
import qualified Enecuum.Blockchain.Domain            as D
import qualified Enecuum.Blockchain.Domain.KBlock     as New
import qualified Enecuum.Core.Interpreters            as I
import qualified Enecuum.Legacy.Refact.Assets         as Old (genesisKeyBlock)
import qualified Enecuum.Legacy.Refact.Hashing        as Old (calculateKeyBlockHash)
import qualified Enecuum.Legacy.Service.Types         as Old (KeyBlockInfoPoW (..))
import           Enecuum.Prelude
import           Test.Hspec                           (Spec, describe, shouldBe)
import           Test.Hspec.Contrib.HUnit             (fromHUnitTest)
import           Test.HUnit                           (Test (..))

spec :: Spec
spec = do
    describe "Key block hash calculation" $ fromHUnitTest $ TestList
        [ TestLabel "Hash calculation for genesis key block: legacy and new are the same" testGenesisKblockHash
        , TestLabel "Hash calculation for random key blocks: legacy and new are the same" testKblockHash]

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

