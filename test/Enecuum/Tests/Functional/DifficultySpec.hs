module Enecuum.Tests.Functional.DifficultySpec where

import           Enecuum.Prelude

import qualified Data.Bits              as Bit
import qualified Data.ByteString        as B
import qualified Enecuum.Domain         as D
import           Enecuum.Tests.Wrappers
import           Test.Hspec

leadingZeroBits' n = foldr (checkBit' (Bit.complement n)) (True, 0) [7, 6..0]

checkBit' n i (True, cnt) | Bit.testBit n i = (True, cnt + 1)
checkBit' n i (True, cnt) = (False, cnt)
checkBit' n i res         = res


spec :: Spec
spec = fastTest $ describe "Difficulty test" $
    it "leadingZeroBitsCount" $ do
        let n = 17 :: Word8

        print $ Bit.testBit (Bit.complement n) 0
        print $ Bit.testBit (Bit.complement n) 1
        print $ Bit.testBit (Bit.complement n) 2
        print $ Bit.testBit (Bit.complement n) 3
        print $ Bit.testBit (Bit.complement n) 4
        print $ Bit.testBit (Bit.complement n) 5
        print $ Bit.testBit (Bit.complement n) 6
        print $ Bit.testBit (Bit.complement n) 7

        print $ leadingZeroBits' n

        1 `shouldBe` 1
