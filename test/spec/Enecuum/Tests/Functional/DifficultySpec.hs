module Enecuum.Tests.Functional.DifficultySpec where

import           Enecuum.Prelude

import qualified Data.Bits              as Bit
import qualified Data.ByteString        as B
import qualified Enecuum.Domain         as D
import           Enecuum.Testing.Wrappers
import           Test.Hspec

import qualified Enecuum.Samples.Blockchain.Language                        as L
import qualified Enecuum.Samples.Blockchain.Domain                          as D

spec :: Spec
spec = stableTest $ fastTest $ describe "Difficulty test" $ do
    it "Count leading Zero Bits" $ do
        D.leadingZeroBitsCount 0  `shouldBe` 8
        D.leadingZeroBitsCount 1  `shouldBe` 7
        D.leadingZeroBitsCount 16 `shouldBe` 3
        D.leadingZeroBitsCount 17 `shouldBe` 3

        map D.leadingZeroBitsCount [(2 ^ i) :: Word8 | i <- [0..7]]
          `shouldBe` [7,6..0]

    it "Calculate hash difficulty" $ do
        D.calcHashDifficulty (D.RawHash $ B.pack [1, 121]) `shouldBe` 7
        D.calcHashDifficulty (D.RawHash $ B.pack [0, 1]) `shouldBe` 15

    it "Calculate some hashes difficulty" $ do
        D.calcHashDifficulty (D.RawHash $ B.pack [246,2,161,0,0])   `shouldBe` 0
        D.calcHashDifficulty (D.RawHash $ B.pack [0,246,2,161,0,0]) `shouldBe` 8
        D.calcHashDifficulty (D.RawHash $ B.pack [0,0,0,0,0,0])     `shouldBe` 48
